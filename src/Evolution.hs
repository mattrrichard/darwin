{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Evolution
       ( Individual (..)
       , EvolutionStrategy (..)
       , compareFitness
       , evolve
       ) where

import Data.Foldable (minimumBy)
import Data.Maybe
import Data.Random
import Data.List
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Generics

class Monad m => Individual m a | a -> m where
  fitness :: a -> Double
  mutate :: a -> m a
  recombine :: a -> a -> m a

class EvolutionStrategy s where
  joinGens :: Individual m a => s -> [a] -> [a] -> [a]
  breed :: Individual m a => s -> [a] -> m [a]

data Cached a
  = Cached a Double
  | Uncached a
  deriving (Show, Generic)

instance NFData a => NFData (Cached a)

indFromCached (Cached a _) = a
indFromCached (Uncached a) = a


instance Individual m a => Individual m (Cached a) where
  fitness (Cached _ fit) = fit
  fitness (Uncached a) = fitness a

  mutate = fmap Uncached . mutate . indFromCached

  recombine a b = Uncached <$> recombine (indFromCached a) (indFromCached b)


ensureCached (Uncached a) = Cached a (fitness a)
ensureCached c = c


step :: (Individual m a, EvolutionStrategy s, NFData a) => s -> [Cached a] -> m [Cached a]
step s pop = do
  let cachedPop = ensureCached <$> pop `using` parListChunk 32 rdeepseq
  nextGen <- breed s cachedPop
  return $ joinGens s cachedPop nextGen


evolve :: (EvolutionStrategy s, Individual m a, NFData a) => s -> [a] -> [m [a]]
evolve s startingPop =
  indFromCached `f3` cachedGens
  where
    f3 = fmap . fmap . fmap -- list . m . list

    initial = map Uncached startingPop

    cachedGens =
      iterate (>>= (step s)) $ return initial


compareFitness :: Individual m a => a -> a -> Ordering
compareFitness a b =
  compare (fitness a) (fitness b)

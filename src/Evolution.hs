{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Evolution
       ( Individual (..)
       , EvolutionStrategy (..)
       , compareFitness
       , evolve
       ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Foldable               (minimumBy)
import           Data.List
import           Data.Maybe
import           Data.Random
import           GHC.Generics


class (Monad m, NFData a) => Individual m a | a -> m where
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


step :: (Individual m a, EvolutionStrategy s) => s -> [Cached a] -> m [Cached a]
step s pop = do
  let cachedPop = cache pop
  children <- cache <$> breed s cachedPop

  let nextGen = joinGens s cachedPop children

  return $ sortBy compareFitness nextGen

  where
    cache p =
      if all isCached p then p
      else ensureCached <$> p `using` parListChunk 16 rdeepseq

    isCached (Uncached _) = False
    isCached _ = True


evolve :: (EvolutionStrategy s, Individual m a) => s -> [a] -> [m [a]]
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

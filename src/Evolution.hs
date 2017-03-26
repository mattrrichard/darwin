{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Evolution
       ( Individual (..)
       , EvolutionStrategy (..)
       , compareFitness
       , evolve
       ) where

import           Control.Arrow               ((&&&))
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans         (lift)
import           Control.Parallel.Strategies
import           Data.Foldable               (minimumBy)
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Random
import           GHC.Generics
import           Pipes
import qualified Pipes.Prelude               as P


class NFData a => Individual a where
  fitness :: a -> Double
  mutate :: MonadRandom m => a -> m a
  recombine :: MonadRandom m => a -> a -> m a


class EvolutionStrategy s where
  popSize :: s -> Int
  joinGens :: Individual a => s -> [a] -> [a] -> [a]
  breed :: (Individual a, MonadRandom m) => s -> [a] -> m [a]


data Cached a
  = Cached !a Double
  | Uncached a
  deriving (Show, Generic)

instance NFData a => NFData (Cached a)

indFromCached (Cached a _) = a
indFromCached (Uncached a) = a


instance Individual a => Individual (Cached a) where
  fitness (Cached _ fit) = fit
  fitness (Uncached a) = fitness a

  mutate = fmap Uncached . mutate . indFromCached

  recombine a b = Uncached <$> recombine (indFromCached a) (indFromCached b)


ensureCached (Uncached a) = Cached a (fitness a)
ensureCached c = c


step :: (Individual a, EvolutionStrategy s, MonadRandom m) => s -> [Cached a] -> m [Cached a]
step s pop = do
  let cachedPop = cache pop
  children <- cache <$> breed s cachedPop

  let nextGen = joinGens s cachedPop children

  return $ sortBy compareFitness nextGen

  where
    cache p =
      if all isCached p then p
      else ensureCached <$> p `using` parList rdeepseq

    isCached (Uncached _) = False
    isCached _ = True


evolve :: (EvolutionStrategy s, Individual a, MonadRandom m) => s -> [a] -> Producer [a] m ()
evolve s startingPop =
  P.unfoldr evoStep (Uncached <$> startingPop)
  where evoStep = fmap wrapNextGen . step s
        wrapNextGen = Right . (map indFromCached &&& id)


compareFitness :: Individual a => a -> a -> Ordering
compareFitness =
  compare `on` fitness



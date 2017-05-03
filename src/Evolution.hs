{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Evolution
       ( HasFitness (..)
       , Tweakable (..)
       , Individual
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
import           Data.Ord                    (Down(..))
import           Data.Random
import           GHC.Generics
import           Pipes
import qualified Pipes.Prelude               as P


class (Ord (Fitness a), NFData (Fitness a)) => HasFitness a where
  type Fitness a :: *
  fitness :: a -> Fitness a

class Tweakable m a where
  type TweakConfig a :: *
  generate :: TweakConfig a -> m a
  mutate :: TweakConfig a -> a -> m a

class (HasFitness a, Tweakable m a) => Individual m a where

newtype Cached a b = Cached { getCached :: (a, b) } deriving Show

uncache = fst . getCached

cache :: HasFitness a => a -> Cached a (Fitness a)
cache = Cached . (id &&& fitness)

instance (HasFitness a, Fitness a ~ b) => HasFitness (Cached a b) where
  type Fitness (Cached a b) = b
  fitness = snd . getCached

instance (Individual m a, Fitness a ~ b, Functor m) => Tweakable m (Cached a b) where
  type TweakConfig (Cached a b) = TweakConfig a
  generate = fmap cache . generate
  mutate c = fmap cache . mutate c . uncache

instance (Individual m a, Fitness a ~ b, Functor m) => Individual m (Cached a b)

class EvolutionStrategy s where
  popSize :: s -> Int
  joinGens :: HasFitness a => s -> [a] -> [a] -> [a]
  breed :: (Individual m a, MonadRandom m) => s -> [a] -> TweakConfig a -> m [a]


step :: (Individual m a, EvolutionStrategy s, MonadRandom m, Fitness a ~ b)
     => s
     -> TweakConfig a
     -> [Cached a b]
     -> m [Cached a b]
step s tweakConfig pop = do
  children <- breed s pop tweakConfig
  let children' = (map fitness children `using` parList rdeepseq) `seq` children

  let nextGen = joinGens s pop children'

  return $ sortBy compareFitness nextGen


evolve :: (EvolutionStrategy s, Individual m a, MonadRandom m)
  => s
  -> TweakConfig a
  -> [a] -> Producer [a] m ()
evolve s tweakConfig startingPop =
  P.unfoldr evoStep (cache <$> startingPop)
  where evoStep = fmap wrapNextGen . step s tweakConfig
        wrapNextGen = Right . (map uncache &&& id)

compareFitness :: HasFitness a => a -> a -> Ordering
compareFitness =
  compare `on` (Down . fitness)

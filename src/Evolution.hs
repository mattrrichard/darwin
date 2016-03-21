{-# LANGUAGE RankNTypes #-}
module Evolution
       ( Individual
       , compareFitness
       , evolve
       ) where

import Data.Foldable (minimumBy)
import Data.Maybe
import Data.Random
import Data.List

class Individual a where
  fitness :: a -> Double
  genome :: a -> b

  fromGenome :: Individual a => b -> a


data CachedFitnessIndividual a =
  Ind { individual :: a
      , cachedFitness :: Maybe Double
      }

instance Individual a => Individual (CachedFitnessIndividual a) where
  fitness x =
    fromMaybe (fitness (individual x)) (cachedFitness x)

  genome =
    genome . individual

  fromGenome x =
    Ind ind (Just $ fitness ind)
    where ind = fromGenome x


compareFitness :: Individual a => a -> a -> Ordering
compareFitness a b =
  compare (fitness a) (fitness b)


makeCached :: Individual a => a -> CachedFitnessIndividual a
makeCached x =
  Ind x (Just $ fitness x)


evolve :: Individual a =>
          (forall b . Individual b => [b] -> [b] -> [b])
       -> (forall b . Individual b => [b] -> RVar [b])
       -> Int
       -> [a]
       -> RVar a
evolve join breed maxGens initialPop =
  step maxGens initialPop' initialBest
  where
    initialPop' = map makeCached initialPop
    initialBest = head initialPop'

    step 0 _ best = return $ individual best
    step n pop best = do
      let best' = minimumBy compareFitness (best : pop)

      nextGen <- breed pop

      let pop' = join pop nextGen

      step (n-1) pop' best'


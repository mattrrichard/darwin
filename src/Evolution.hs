module Evolution where

import Data.Foldable (minimumBy)
import Data.Random

data Individual a =
  Individual { indGenome  :: a
             , indFitness :: Double
             }

type Population a = [Individual a]


assess :: (a -> Double) -> a -> Individual a
assess fitness genome =
  Individual genome (fitness genome)


compareFitness :: Individual a -> Individual a -> Ordering
compareFitness a b =
  compare (indFitness a) (indFitness b)


evolve :: (a -> Double) -> (Population a -> [a] -> [a]) -> (Population a -> RVar [a]) -> Int -> [a] -> RVar a
evolve fitness join breed maxGens genomes =
  step maxGens genomes initialBest
  where
    initialBest = assess' (head genomes)
    assess' = assess fitness

    step 0 _ best = return (indGenome best)
    step n popGenomes best = do
      let pop = map assess' popGenomes
      let best' = minimumBy compareFitness (best : pop)

      nextGen <- breed pop

      let popGenomes' = join pop nextGen

      step (n-1) popGenomes' best'

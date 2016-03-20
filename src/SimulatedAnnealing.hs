module SimulatedAnnealing where

import Data.Random

-- | Simulated Annealing search.  Basically hill-climbing with a twist:
-- the @initialT@ variable (representing temperature) adds a chance for the search
-- choose the candidate even when it is not better than its parent while the
-- value is large.  On each iteration, the temperature is adjusted (usually down)
-- via the @dt@ parameter.  The result is a search that is initially mostly a random
-- walk that gradually changes into simple hill-climbing
simulatedAnnealing
  :: (a -> RVar a) -- ^ tweak a candidate to produce a new candidate
  -> (a -> Double) -- ^ measure the quality of the candidate (lower is better)
  -> Int           -- ^ number of generations to produce
  -> Double        -- ^ starting value of t
  -> Double        -- ^ amount to change t each iteration (typically negative)
  -> a             -- ^ the initial candidate
  -> RVar a
simulatedAnnealing tweak quality maxGens initialT dt initial =
  sim maxGens initialT initial (quality initial) initial (quality initial)
  where
    sim 0 _ _ _ best _ = return best
    sim n t s qs best qbest = do
      r <- tweak s
      p <- stdUniform

      let qr = quality r

      let (best', qbest') =
            if qr < qbest then
              (r, qr)
            else
              (best, qbest)

      let pReplaceAnyway = exp ((qs - qr) / t)

      let (s', qs') =
            if qr < qs || p < pReplaceAnyway then
              (r, qr)
            else
             (s, qs)

      let t' = max 0 (t + dt)

      sim (n-1) t' s' qs' best' qbest'

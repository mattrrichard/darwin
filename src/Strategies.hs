module Strategies where

import Evolution
import Data.List
import Control.Monad

data MuLambda = MuLambda Int Int

instance EvolutionStrategy MuLambda where
  joinGens _ _ children = children

  breed (MuLambda mu lambda) parents = do
    let selected = take mu $ sortBy compareFitness parents

    children <- mapM makeChildren selected

    return $ mconcat children

    where
      makeChildren parent =
        replicateM childrenPerParent (mutate parent)

      childrenPerParent = lambda `div` mu

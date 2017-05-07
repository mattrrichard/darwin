{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Strategies where

import           Control.Monad
import           Data.List
import           Data.Random
import           Data.Random.List
import           Evolution


data MuLambda = MuLambda Int Int

instance EvolutionStrategy MuLambda where
  popSize (MuLambda _ lambda) = lambda

  joinGens _ _ children = children

  breed (MuLambda mu lambda) parents config = do
    let selected = take mu $ sortBy compareFitness parents

    children <- mapM makeChildren selected

    return $ mconcat children

    where
      makeChildren parent =
        replicateM childrenPerParent (mutate config parent)

      childrenPerParent = lambda `div` mu


data MuPlusLambda = MuPlusLambda Int Int

instance EvolutionStrategy MuPlusLambda where
  popSize (MuPlusLambda _ lambda) = lambda

  breed (MuPlusLambda mu lambda) = breed (MuLambda mu lambda)

  joinGens (MuPlusLambda mu lambda) parents children =
    take mu (sortBy compareFitness $ parents ++ children)


tournamentSelection :: HasFitness a => Int -> [a] -> RVar a
tournamentSelection t pop =
  randomElement pop >>= runTournament (t-1)

  where
    runTournament t currentWinner | t > 0 = do
      challenger <- randomElement pop
      runTournament (t-1) (minimumBy compareFitness [currentWinner, challenger])

    runTournament _ currentWinner = return currentWinner


-- data TournamentElitism = TournamentElitism Int Int Int

-- instance EvolutionStrategy RVar TournamentElitism where
--   popSize (TournamentElitism lambda _ _) = lambda

--   joinGens (TournamentElitism _ _ elites) parents children =
--     take elites (sortBy compareFitness parents) ++ children

--   breed (TournamentElitism lambda t _) parents =
--     replicateM lambda (tournamentSelection t parents)
--     >>= mapM mutate

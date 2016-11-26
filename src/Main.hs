{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           CircleImage
import           Codec.Picture
import           Control.Monad
import           Data.List
import           Data.Random
import qualified Data.Vector.Storable as V
import           Evolution
import           Graphics.Rasterific
import           SimulatedAnnealing
import           Strategies



main :: IO ()
main = do
  (Right source) <- readImage "images/landscape-st-remy-306-240.jpg"
  let sourceImg = convertRGBA8 source

  -- previous <- readCircles <$> readFile "best.circles"
  -- let initial = map (CircleImage sourceImg) previous

  initial <- runRVar (replicateM 64 $ circleImageGen 50 sourceImg) StdRandom

  let generations = evolve (MuLambda 16 64) initial
  lastGen <- runRVar (last . take 5 $ generations) StdRandom
  let best = circles $ minimumBy compareFitness lastGen

  writePng "source.png" sourceImg
  writePng "best.png" $ render best

  -- writeFile "best.circles" (show (map circles lastGen))

  where

    readCircles :: String -> [[Circle]]
    readCircles = read

    circles (CircleImage _ cs) = cs

    render =
      renderWhite 306 240 . renderCircles

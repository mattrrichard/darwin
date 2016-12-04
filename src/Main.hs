{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           CircleImage
import           Codec.Picture
import           Control.Monad
import           Control.Monad.Extra
import           Data.List
import           Data.Random
import qualified Data.Vector.Storable as V
import           Evolution
import           Graphics.Rasterific
import           SimulatedAnnealing
import           Strategies
import           System.IO

main = do
  (Right source) <- readImage "images/landscape-st-remy-306-240.jpg"
  let sourceImg = convertRGBA8 source

  previous <- readCircles <$> readFile "best.circles"
  let initial = map (CircleImage sourceImg) previous

  iterateM (loop sourceImg 25) (175, initial)

  where
    readCircles :: String -> [[Circle]]
    readCircles = read

loop :: Image PixelRGBA8 -> Int -> (Int, [CircleImage]) -> IO (Int, [CircleImage])
loop sourceImg stepCount (genCount, previous) = do

  putStrLn $ "starting generation " ++ show genCount
  hFlush stdout

  initial <- runRVar (replicateM 64 $ circleImageGen 50 sourceImg) StdRandom

  let generations = evolve (MuPlusLambda 16 64) initial
  lastGen <- runRVar (last . take stepCount $ generations) StdRandom
  let best = circles $ minimumBy compareFitness lastGen

  writePng ("best" ++ show genCount ++ ".png") $ render best

  writeFile ("best" ++ show genCount ++ ".circles") (show (map circles lastGen))

  return (genCount + stepCount, lastGen)

  where

    readCircles :: String -> [[Circle]]
    readCircles = read

    circles (CircleImage _ cs) = cs

    render =
      renderWhite 306 240 . renderCircles

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

main :: IO ()
main = do
  (Right source) <- readImage "images/landscape-st-remy-306-240.jpg"
  let sourceImg = convertRGBA8 source

  let startGenId = 1600

  initial <- getGen sourceImg 50 startGenId

  iterateM (loop sourceImg 25) (startGenId, initial)

  return ()

  where
    getGen sourceImg size 0 =
      runRVar (replicateM size $ circleImageGen 50 sourceImg) StdRandom

    getGen sourceImg _ id =
      map (CircleImage sourceImg) . readCircles
      <$> readFile ("out/generation" ++ show id ++ ".circles")

    readCircles :: String -> [[Circle]]
    readCircles = read


loop :: Image PixelRGBA8 -> Int -> (Int, [CircleImage]) -> IO (Int, [CircleImage])
loop sourceImg stepCount (genId, initial) = do

  putStrLn $ "starting from generation " ++ show genId
  hFlush stdout

  let generations = evolve (MuPlusLambda 8 64) initial

  lastGen <- runRVar (last . take stepCount $ generations) StdRandom

  let best = head lastGen
  let endingGenId = genId + stepCount

  writePng ("out/best" ++ show endingGenId ++ ".png") $ render best

  writeFile ("out/generation" ++ show endingGenId ++ ".circles") (show (map circles lastGen))

  return (endingGenId, lastGen)

  where

    readCircles :: String -> [[Circle]]
    readCircles = read

    circles (CircleImage _ cs) = cs

    render (CircleImage (Image w h _) circles) =
      renderWhite w h $ renderCircles circles

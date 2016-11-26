module Main where

import Codec.Picture
import Graphics.Rasterific
import CircleImage
import SimulatedAnnealing
import qualified Data.Vector.Storable as V
import Data.Random

fitness :: Image PixelRGBA8 -> Image PixelRGBA8 -> Double
fitness (Image _ _ source) (Image _ _ target) =
  V.sum $ V.zipWith deltaSq source target
  where
    deltaSq a b =
      delta * delta
      where delta = fromIntegral a / 255 - fromIntegral b / 255


circleImageFitness :: Int -> Int -> Image PixelRGBA8 -> CircleImage -> Double
circleImageFitness  w h source =
  fitness source . renderWhite w h . renderCircles


renderWhite :: Int -> Int -> Drawing PixelRGBA8 () -> Image PixelRGBA8
renderWhite w h =
  renderDrawing w h white
  where
    white = PixelRGBA8 255 255 255 255


main :: IO ()
main = do
  -- TODO: Stop being lazy and handle the possible failure here
  (Right source) <- readImage "images/landscape-st-remy-306-240.jpg"
  let sourceImg = convertRGBA8 source

  initial <- runRVar (circleImageGen 50 306 240) StdRandom

  best <- runRVar (runSim 500 sourceImg initial) StdRandom

  writePng "source.png" sourceImg
  writePng "initial.png" $ render initial
  writePng "best.png" $ render best

  where
    render =
      renderWhite 306 240 . renderCircles

    runSim maxGens source =
      simulatedAnnealing tweak quality maxGens 100 (-0.25)
      where
        tweak = tweakCircleImage 5 16
        quality = circleImageFitness  306 240 source

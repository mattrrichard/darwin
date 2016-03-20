module Main where

import Codec.Picture
import Graphics.Rasterific
import CircleImage
import SimulatedAnnealing
import qualified Data.Vector.Storable as V
import Data.Random

main :: IO ()
main = testSim


fitness :: Image PixelRGBA8 -> Image PixelRGBA8 -> Double
fitness (Image _ _ source) (Image _ _ target) =
  sum $ zipWith deltaSq (V.toList source) (V.toList target)
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


runSim :: Int -> Image PixelRGBA8 -> CircleImage -> RVar CircleImage
runSim maxGens source =
  simulatedAnnealing tweak quality maxGens 100 (-0.25)
  where
    tweak = tweakCircleImage 5
    quality = circleImageFitness  306 240 source


testSim :: IO ()
testSim = do
  -- source <- runRVar (circleImageGen 50 400 400) StdRandom
  -- let sourceImg = render source

  (Right source) <- readImage "images/landscape-st-remy-306-240.jpg"
  let sourceImg = convertRGBA8 source

  initial <- runRVar (circleImageGen 50 306 240) StdRandom

  best <- runRVar (runSim 25000 sourceImg initial) StdRandom

  writePng "source.png" sourceImg
  writePng "initial.png" $ render initial
  writePng "best.png" $ render best

  where
    render =
      renderWhite 306 240 . renderCircles

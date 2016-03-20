module Main where

import Codec.Picture
import Graphics.Rasterific
import CircleImage
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


drawingFitness :: Int -> Int -> Image PixelRGBA8 -> CircleImage -> Double
drawingFitness w h source =
  fitness source . renderWhite w h . renderCircles



simulatedAnnealing :: (a -> RVar a) -> (a -> Double) -> Int -> Double -> Double -> a -> RVar a
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


renderWhite :: Int -> Int -> Drawing PixelRGBA8 () -> Image PixelRGBA8
renderWhite w h =
  renderDrawing w h white
  where
    white = PixelRGBA8 255 255 255 255


diffImages :: Pixel a => Image a -> Image a -> Image PixelRGBA8
diffImages img1@(Image w1 h1 _) img2@(Image w2 h2 _) =
  generateImage diff' w h
  where
    w = max w1 w2
    h = max h1 h2
    diff' x y =
      if p1 == p2 then green else red
      where
        p1 = pixelAt img1 x y
        p2 = pixelAt img2 x y

    green = PixelRGBA8   0 200   0 255
    red   = PixelRGBA8 200   0   0 255


renderTest :: IO ()
renderTest = do
  circles <- runRVar (circleImageGen 50 400 400) StdRandom

  circles2 <- runRVar (tweakCircleImage 3 circles) StdRandom

  let img1 = render circles
  let img2 = render circles2

  writePng "test.png" img1
  writePng "test2.png" img2

  where
    render =
      renderWhite 400 400 . renderCircles


runSim :: Int -> Image PixelRGBA8 -> CircleImage -> RVar CircleImage
runSim maxGens source =
  simulatedAnnealing tweak quality maxGens 100 (-0.25)
  where
    tweak = tweakCircleImage 5
    quality = drawingFitness 306 240 source


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

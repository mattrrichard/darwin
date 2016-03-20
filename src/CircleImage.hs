{-# LANGUAGE FlexibleContexts #-}
module CircleImage where

import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import qualified Data.Vector.Storable as V

data Circle =
  Circle { circleX :: Int
         , circleY :: Int
         , circleR :: Int
         , circleColor :: PixelRGBA8
         }

type CircleImage = [Circle]

renderCircle :: Circle -> Drawing PixelRGBA8 ()
renderCircle (Circle x y r c) =
  withTexture tex . fill $ circle pos radius
  where
    tex = uniformTexture c
    pos = V2 (fromIntegral x) (fromIntegral y)
    radius = fromIntegral r


renderCircles :: CircleImage -> Drawing PixelRGBA8 ()
renderCircles = mapM_ renderCircle


testCircles =
  [ Circle 30 30 15 (PixelRGBA8 178 40 255 255)
  , Circle 60 60 15 (PixelRGBA8 78 40 255 255)
  , Circle 90 90 45 (PixelRGBA8 0 40 25 55)
  ]

translateCircle dx dy (Circle x y r c) =
  Circle (x + dx) (y + dy) r c


test1 =
  renderWhite 1280 1014 $
  renderCircles testCircles

test2 =
  renderWhite 1280 1014 $
  renderCircles (map (translateCircle 10 10) testCircles)

test3 =
  renderWhite 1280 1014 $
  renderCircles (map (translateCircle 10 50) testCircles)

renderWhite w h drawing =
  renderDrawing w h white drawing
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


fitness :: Image PixelRGBA8 -> Image PixelRGBA8 -> Double
fitness source@(Image w h _) target =
  raw
  where
    raw =
      sum [diff (s x y) (t x y) | x <- [0..w-1]
                                , y <- [0..h-1]]
    s = pixelAt source
    t = pixelAt target

    (-.) cs ct =
      delta * delta
      where
        delta = fromIntegral cs / 255 - fromIntegral ct / 255

    diff :: PixelRGBA8 -> PixelRGBA8 -> Double
    diff (PixelRGBA8 r g b _) (PixelRGBA8 r' g' b' _) =
      r -. r' +
      g -. g' +
      b -. b'


fitness' :: Image PixelRGBA8 -> Image PixelRGBA8 -> Double
fitness' (Image _ _ source) (Image _ _ target) =
  sum $ zipWith deltaSq (V.toList source) (V.toList target)
  where
    deltaSq a b =
      delta * delta
      where delta = fromIntegral a / 255 - fromIntegral b / 255


fitness'' :: Image PixelRGBA8 -> Image PixelRGBA8 -> Double
fitness'' (Image _ _ source) (Image _ _ target) =
  V.ifoldl' worker 0 source
  where
    worker acc i x = acc + deltaSq x (target V.! i)
    deltaSq x x' =
      let delta = toD x - toD x'
      in delta * delta
    toD x = fromIntegral x / 255

renderTest :: IO ()
renderTest =
  writePng "test.png" (diffImages test1 test2)


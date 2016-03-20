{-# LANGUAGE FlexibleContexts #-}
module CircleImage where

import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture
-- import qualified Data.Vector.Storable as V

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


fitness :: Image PixelRGBA8 -> Image PixelRGBA8 -> Float
fitness source@(Image w h _) target =
  1.0 - (fromInteger raw / fromIntegral maxError)
  where
    raw =
      sum [abs $ diff (s x y) (t x y) | x <- [0..w-1]
                                      , y <- [0..h-1]]
    maxError =
      w * h * 4 * 255

    s = pixelAt source
    t = pixelAt target

    (-.) cs ct =
      toInteger cs - toInteger ct

    diff :: PixelRGBA8 -> PixelRGBA8 -> Integer
    diff (PixelRGBA8 r g b a) (PixelRGBA8 r' g' b' a') =
      r -. r' +
      g -. g' +
      b -. b' +
      a -. a'


renderTest :: IO ()
renderTest =
  writePng "test.png" (diffImages test1 test2)


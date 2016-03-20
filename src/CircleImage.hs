module CircleImage where

import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.Random
import Control.Monad

import Util

data Circle =
  Circle { circleX :: Int
         , circleY :: Int
         , circleR :: Int
         , circleColor :: PixelRGBA8
         }
  deriving (Show)


type CircleImage = [Circle]


circleImageGen :: Int -> Int -> Int -> RVar CircleImage
circleImageGen maxCircles w h = do
  -- n <- uniform 1 maxCircles
  let n = maxCircles
  replicateM n $ circleGen w h


circleGen :: Int -> Int -> RVar Circle
circleGen imageW imageH = do
  r <- (+15) <$> normalInt 30
  x <- uniform (-r) (imageW+r-1)
  y <- uniform (-r) (imageH+r-1)

  c <- uniformColor

  return $ Circle x y r c

  where
    uniformColor = PixelRGBA8 <$> col <*> col <*> col <*> col
    col = uniform 0 255


tweakCircleImage :: Int -> Int -> CircleImage -> RVar CircleImage
tweakCircleImage s sc =
  mapM (tweakCircle s sc)


tweakCircle :: Int -> Int -> Circle -> RVar Circle
tweakCircle s sc (Circle x y r c) = do
  dx <- normalInt s
  dy <- normalInt s
  dr <- normalInt s

  c' <- tweakPixelRGBA8 sc c

  return $ Circle (x+dx) (y+dy) (r+dr) c'


renderCircle :: Circle -> Drawing PixelRGBA8 ()
renderCircle (Circle x y r c) =
  withTexture tex . fill $ circle pos radius
  where
    tex = uniformTexture c
    pos = V2 (fromIntegral x) (fromIntegral y)
    radius = fromIntegral r


renderCircles :: CircleImage -> Drawing PixelRGBA8 ()
renderCircles = mapM_ renderCircle



module CircleImage where

import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.Random
import Control.Monad


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


normalInt :: Int -> RVar Int
normalInt s = do
  x <- stdNormal :: RVar Float
  return (round (x * fromIntegral s))


tweakCircleImage :: Int -> CircleImage -> RVar CircleImage
tweakCircleImage s =
  mapM (tweakCircle s)

tweakCircle :: Int -> Circle -> RVar Circle
tweakCircle s (Circle x y r c) = do
  dx <- normalInt s
  dy <- normalInt s
  dr <- normalInt s

  c' <- tweakColor c

  return $ Circle (x+dx) (y+dy) (r+dr) c'

  where
    tweakColor (PixelRGBA8 r g b a) = do
      r' <- colorDelta r
      g' <- colorDelta g
      b' <- colorDelta b
      a' <- colorDelta a

      return $ PixelRGBA8 r' g' b' a'

    colorDelta x = do
      dx <- normalInt 64
      return $ clamp (fromIntegral x + dx)

    clamp x = fromIntegral $ max 0 (min 255 x)


renderCircle :: Circle -> Drawing PixelRGBA8 ()
renderCircle (Circle x y r c) =
  withTexture tex . fill $ circle pos radius
  where
    tex = uniformTexture c
    pos = V2 (fromIntegral x) (fromIntegral y)
    radius = fromIntegral r


renderCircles :: CircleImage -> Drawing PixelRGBA8 ()
renderCircles = mapM_ renderCircle



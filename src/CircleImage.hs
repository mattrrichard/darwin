{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module CircleImage where

import           Codec.Picture
import           Control.DeepSeq
import           Control.Monad
import           Data.Random
import qualified Data.Vector.Storable        as V
import           Evolution
import           GHC.Generics
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture
import           Util
import           ImageUtils


data Circle =
  Circle { circleX     :: Int
         , circleY     :: Int
         , circleR     :: Int
         , circleColor :: PixelRGBA8
         }
  deriving (Read, Show, Generic)

instance NFData Circle

data CircleImage =
  CircleImage (Image PixelRGBA8) [Circle]
  deriving (Generic)

instance NFData CircleImage

instance Show CircleImage where
  show (CircleImage _ circles) = show circles

instance Individual CircleImage where
  fitness = circleImageFitness
  mutate = sample .tweakCircleImage 2.5 16
  recombine x y = return x


circleImageGen :: Int -> Image PixelRGBA8 -> RVar CircleImage
circleImageGen maxCircles original@(Image w h _) = do
  -- n <- uniform 1 maxCircles
  let n = maxCircles
  circles <- replicateM n $ circleGen w h
  return $ CircleImage original circles


circleGen :: Int -> Int -> RVar Circle
circleGen imageW imageH = do
  r <- (+15) <$> normalInt 30
  x <- uniform (-r) (imageW+r-1)
  y <- uniform (-r) (imageH+r-1)

  c <- uniformColor

  return $ Circle x y r c



tweakCircleImage :: Float -> Float -> CircleImage -> RVar CircleImage
tweakCircleImage s sc (CircleImage original@(Image w h _) circles) =
  mapM (maybeTweak 0.20 $ tweakCircle s sc) circles
  >>= maybeTweak 0.05 (addItem $ circleGen w h)
  >>= maybeTweak 0.01 randomRemove
  >>= return . CircleImage original


tweakCircle :: Float -> Float -> Circle -> RVar Circle
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


renderCircleImage (CircleImage (Image w h _) circles) =
  renderWhite w h $ mapM_ renderCircle circles

circleImageFitness :: CircleImage -> Double
circleImageFitness ci@(CircleImage original _) =
    imageFitness original $ renderCircleImage ci

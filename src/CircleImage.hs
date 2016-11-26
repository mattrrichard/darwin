{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}

module CircleImage where

import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.Random
import Control.Monad
import qualified Data.Vector.Storable as V
import Graphics.Rasterific
import Evolution
import Util
import GHC.Generics
import Control.DeepSeq

deriving instance Read PixelRGBA8
deriving instance Generic PixelRGBA8
instance NFData PixelRGBA8

data Circle =
  Circle { circleX :: Int
         , circleY :: Int
         , circleR :: Int
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

  where
    uniformColor = PixelRGBA8 <$> col <*> col <*> col <*> col
    col = uniform 0 255


tweakCircleImage :: Int -> Int -> CircleImage -> RVar CircleImage
tweakCircleImage s sc (CircleImage original circles) =
  CircleImage original <$> mapM (tweakCircle s sc) circles


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


renderCircles :: [Circle] -> Drawing PixelRGBA8 ()
renderCircles = mapM_ renderCircle


imageFitness :: Image PixelRGBA8 -> Image PixelRGBA8 -> Double
imageFitness (Image _ _ source) (Image _ _ target) =
  V.sum $ V.zipWith deltaSq source target
  where
    deltaSq a b =
      delta * delta
      where delta = fromIntegral a / 255 - fromIntegral b / 255


circleImageFitness :: CircleImage -> Double
circleImageFitness (CircleImage original@(Image w h _) circles) =
    imageFitness original $ renderWhite w h $ renderCircles circles


renderWhite :: Int -> Int -> Drawing PixelRGBA8 () -> Image PixelRGBA8
renderWhite w h =
  renderDrawing w h white
  where
    white = PixelRGBA8 255 255 255 255


instance Individual RVar CircleImage where
  fitness = circleImageFitness
  mutate = tweakCircleImage 5 16
  recombine x y = return x

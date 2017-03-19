{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module ImageUtils where

import           Codec.Picture
import           Control.DeepSeq
import qualified Data.Vector.Storable        as V
import           GHC.Generics
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture

deriving instance Read PixelRGBA8
deriving instance Generic PixelRGBA8
instance NFData PixelRGBA8

deriving instance Read a => Read (V2 a)
deriving instance Generic (V2 a)
instance NFData Point


imageFitness :: Image PixelRGBA8 -> Image PixelRGBA8 -> Double
imageFitness (Image _ _ source) (Image _ _ target) =
  V.sum $ V.zipWith deltaSq source target
  where
    deltaSq a b =
      delta * delta
      where delta = fromIntegral a / 255 - fromIntegral b / 255


renderWhite :: Int -> Int -> Drawing PixelRGBA8 () -> Image PixelRGBA8
renderWhite w h =
  renderDrawing w h white
  where
    white = PixelRGBA8 255 255 255 255

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module ImageUtils where

import           Codec.Picture
import           Control.DeepSeq
import qualified Data.Vector.Generic as V
import Data.Vector.Generic ((!))
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
  V.ifoldl' f 0 source
  where
    f acc i x =
          acc + deltaSq x (target ! i)
    deltaSq x y =
      let delta = (fromIntegral x - fromIntegral y) / 255
      in delta * delta


renderWhite :: Int -> Int -> Drawing PixelRGBA8 () -> Image PixelRGBA8
renderWhite w h =
  renderDrawing w h white
  where
    white = PixelRGBA8 255 255 255 255

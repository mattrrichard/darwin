{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module ImageUtils where

import           Codec.Picture
import           Control.DeepSeq
import           Data.Ord                    (Down (..))
import           Data.Vector.Generic         ((!))
import qualified Data.Vector.Generic         as V
import           GHC.Generics
import           GHC.Word                    (Word32, Word8)
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture

deriving instance Read PixelRGBA8
deriving instance Generic PixelRGBA8
instance NFData PixelRGBA8

deriving instance Read a => Read (V2 a)
deriving instance Generic (V2 a)
instance NFData Point


-- NB This is only reliable on images with fewer than 2^22 pixels (2048x2048)
-- once you get bigger than that you can have overflow
imageFitness :: Image PixelRGBA8 -> Image PixelRGBA8 -> Down Word32
imageFitness (Image _ _ source) (Image _ _ target) =
  -- wrap in a Down since GT should mean a better individual, but this score is lower-is-better
  Down $ V.ifoldl' f 0 source
  where
    f acc i x =
          acc + fromIntegral (delta x $ target ! i)

    delta x y | x > y     = x - y
              | otherwise = y - x


renderWhite :: Int -> Int -> Drawing PixelRGBA8 () -> Image PixelRGBA8
renderWhite w h =
  renderDrawing w h white
  where
    white = PixelRGBA8 255 255 255 255

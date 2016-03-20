module Util where

import Data.Random
import Codec.Picture

-- | Normal distribution with mean 0 and standard deviation @s@ in Int
normalInt :: Int -> RVar Int
normalInt s = do
  x <- stdNormal :: RVar Float
  return (round (x * fromIntegral s))


-- | tweak the components of an RGBA pixel via a random value from a
-- normal distribution with mean 0 and standard deviation @s@
tweakPixelRGBA8 :: Int -> PixelRGBA8 -> RVar PixelRGBA8
tweakPixelRGBA8 s (PixelRGBA8 r g b a) = do
  r' <- colorDelta r
  g' <- colorDelta g
  b' <- colorDelta b
  a' <- colorDelta a

  return $ PixelRGBA8 r' g' b' a'

  where
    colorDelta x = do
      dx <- normalInt s
      return $ clamp (fromIntegral x + dx)

    clamp x = fromIntegral $ max 0 (min 255 x)


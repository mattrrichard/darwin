module Util where

import Data.Random
import Codec.Picture
import Control.Monad

-- | Normal distribution with mean 0 and standard deviation @s@ in Int
normalInt :: Float -> RVar Int
normalInt s =
  round <$> normal (0 :: Float) s


-- | tweak the components of an RGBA pixel via a random value from a
-- normal distribution with mean 0 and standard deviation @s@
tweakPixelRGBA8 :: Float -> PixelRGBA8 -> RVar PixelRGBA8
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


maybeTweak :: Float -> (a -> RVar a) -> a -> RVar a
maybeTweak p tweak target = do
  x <- uniform 0 1

  if x < p then
    tweak target
  else
    return target


uniformColor :: RVar PixelRGBA8
uniformColor = PixelRGBA8 <$> col <*> col <*> col <*> col
  where col = uniform 0 255

randomRemove = randomRemoveRespectMin 0

randomRemoveRespectMin :: Int -> [a] -> RVar [a]
randomRemoveRespectMin minRemaining xs | length xs <= minRemaining = return xs
randomRemoveRespectMin minRemaining xs =
    dropAt <$> uniform 0 (length xs) <*> pure xs
  where
    dropAt i xs = take i xs ++ drop (i+1) xs


addItem gen =
  liftM2 (:) gen . return

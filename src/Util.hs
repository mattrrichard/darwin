{-# LANGUAGE LambdaCase #-}
module Util where

import           Codec.Picture
import           Control.Monad
import           Data.Foldable
import           Data.Random

-- | Normal distribution with mean 0 and standard deviation @s@ in Int
normalInt :: Float -> RVar Int
normalInt s =
  round <$> normal (0 :: Float) s


-- tweakPixelRGBA8 :: Float -> PixelRGBA8 -> RVar PixelRGBA8
tweakColor p s (PixelRGBA8 r g b a) = do
  r' <- maybeTweak p colorDelta r
  g' <- maybeTweak p colorDelta g
  b' <- maybeTweak p colorDelta b
  a' <- maybeTweak p colorDelta a

  return $ PixelRGBA8 r' g' b' a'

  where
    colorDelta x = do
      dx <- normalInt s
      return $ clamp (fromIntegral x + dx)

    clamp x = fromIntegral $ max 0 (min 255 x)


-- | tweak the components of an RGBA pixel via a random value from a
-- normal distribution with mean 0 and standard deviation @s@
tweakPixelRGBA8 :: Float -> PixelRGBA8 -> RVar PixelRGBA8
tweakPixelRGBA8 = tweakColor 1.0


maybeTweak :: Float -> (a -> RVar a) -> a -> RVar a
maybeTweak 1.0 tweak target = tweak target
maybeTweak p tweak target = do
  x <- uniform 0 1

  if x < p then
    tweak target
  else
    return target


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p =
  foldrM f ([], [])
  where
    f x (xs, ys) =
      p x >>= \case
        True -> return (x : xs, ys)
        False -> return (xs, x : ys)


reorderItems :: Float -> [a] -> RVar [a]
reorderItems p items =
  partitionM (const test) items
  >>= uncurry randomlyReinsert

  where
    test = (< p) <$> uniform 0 1


randomlyReinsert :: [a] -> [a] -> RVar [a]
randomlyReinsert [] list = return list
randomlyReinsert (x:xs) list = do
  pos <- uniform 0 (length list)
  let (left, right) = splitAt pos list
  randomlyReinsert xs (left ++ (x : right))


uniformColor :: RVar PixelRGBA8
uniformColor = PixelRGBA8 <$> col <*> col <*> col <*> uniform 30 60
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

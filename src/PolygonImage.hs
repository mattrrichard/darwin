{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module PolygonImage where

import           Codec.Picture
import           Control.DeepSeq
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Ord
import           Data.Random
import           Evolution
import           GHC.Generics
import           GHC.Word                    (Word32)
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture
import           ImageUtils
import           Util


data Polygon =
  Polygon [Point] PixelRGBA8
  deriving (Read, Show, Eq, Generic)


data PolygonImage =
  PolygonImage (Image PixelRGBA8) [Polygon]
  deriving (Generic)

instance NFData Polygon
instance NFData PolygonImage

instance Show PolygonImage where
  showsPrec p (PolygonImage _ polygons) = showsPrec p polygons


instance HasFitness PolygonImage where
  type Fitness PolygonImage = Down Word32 -- wrapped in "Down" because small fitness values are better
  fitness = polygonImageFitness

instance MonadRandom m => Tweakable m PolygonImage where
  type TweakConfig PolygonImage = Image PixelRGBA8
  generate = return . initEmpty
  mutate _ = sample . tweakPolygonImage 2.5 16


genPolygonImage :: Int -> Image PixelRGBA8 -> RVar PolygonImage
genPolygonImage polygonCount sourceImg@(Image w h _) =
  PolygonImage sourceImg <$> replicateM polygonCount (genPolygon w h)


initEmpty sourceImg = PolygonImage sourceImg []

genPoint w h =
  V2 <$> uniformF w <*> uniformF h
  where
    uniformF x = uniform 0 (fromIntegral x :: Float)

renderPolygon (Polygon points color) =
  withTexture tex $ fill lines
  where
    tex = uniformTexture color

    lines = zipWith Line points (drop 1 $ cycle points)


renderPolygonImage (PolygonImage (Image w h _) polys) =
  renderWhite w h $ mapM_ renderPolygon polys


tweakPoint :: Float -> Point -> RVar Point
tweakPoint s (V2 x y) =
  V2 <$> tweak x <*> tweak y
  where
    tweak v = (+v) <$> normal 0 s


addPolygon :: Image a -> [Polygon] -> RVar [Polygon]
addPolygon (Image w h _) =
  liftM2 (:) (genPolygon w h) . return


polygonImageFitness :: PolygonImage -> Down Word32
polygonImageFitness pi@(PolygonImage source _) =
  imageFitness source $ renderPolygonImage pi


tweakPolygonImage s sc pi@(PolygonImage source@(Image w h _) polys) = do
  polys' <- mapM (maybeTweak 0.98 $ tweakPolygon s sc w h) polys
            >>= maybeTweak 0.001 (liftM2 (:) (genPolygon w h) . return)
            >>= maybeTweak 0.001 randomRemove
            >>= reorderItems 0.00025

  if polys' == polys
  then tweakPolygonImage s sc pi
  else return $ PolygonImage source polys'


genPolygon :: Int -> Int -> RVar Polygon
genPolygon w h = do
  start <- genPoint w h
  numPoints <- (+3) . abs <$> normalInt 0
  Polygon <$> replicateM numPoints (randomTranslate start) <*> uniformColor

  where
    sx = fromIntegral w / 50
    sy = fromIntegral h / 50

    randomTranslate v = fmap (+ v) (V2 <$> normal 0 sx <*> normal 0 sy)


genPolygonRandomWalk :: Int -> Int -> RVar Polygon
genPolygonRandomWalk w h = do
  start <- genPoint w h
  numPoints <- (+2) . abs <$> normalInt 0
  points <- (start :) <$> go numPoints start
  Polygon points <$> uniformColor

  where
    dx = fromIntegral w / 100
    dy = fromIntegral h / 100

    go 0 _ = return []
    go n (V2 x y) = do
      x' <- (+ x) <$> normal 0 dx
      y' <- (+ y) <$> normal 0 dy

      let p' = V2 x' y'

      (p' :) <$> go (n-1) p'


splitLine :: [Point] -> RVar [Point]
splitLine points = do
  index <- uniform 0 $ length points
  let [before, after] = take 2 . drop (index + length points - 1) . cycle $ points
  let halfway = (before + after) * V2 0.5 0.5
  halfway' <- tweakPoint 1.5 halfway
  return $ take index points ++ [halfway'] ++ drop index points


tweakPolygon s sc w h (Polygon points color) =
  Polygon <$> points' <*> color'
  where
    color' = tweakColor 0.00075 sc color

    points' =
      maybeTweak 0.00015 splitLine points
      >>= mapM (maybeTweak 0.004 $ tweakPoint s)
      >>= maybeTweak 0.0006 (randomRemoveRespectMin 3)

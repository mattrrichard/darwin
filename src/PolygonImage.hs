{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

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
  show (PolygonImage _ polygons) = show polygons


instance HasFitness PolygonImage where
  type Fitness PolygonImage = Down Double -- wrapped in "Down" because small fitness values are better
  fitness = Down . polygonImageFitness

instance MonadRandom m => Tweakable m PolygonImage where
  type TweakConfig PolygonImage = Image PixelRGBA8
  generate = sample . polygonImageGen 10
  mutate _ = sample . tweakPolygonImage' 2.5 16


polygonImageGen :: Int -> Image PixelRGBA8 -> RVar PolygonImage
polygonImageGen polygonCount sourceImg@(Image w h _) =
  PolygonImage sourceImg <$> replicateM polygonCount (genPolygon' w h)


initEmpty sourceImg = PolygonImage sourceImg []

genPolygon :: Int -> Int -> RVar Polygon
genPolygon w h = do
  sides <- (+3) . round . abs <$> (stdNormal :: RVar Float)

  color <- uniformColor

  points <- replicateM sides (genPoint w h)

  return $ Polygon points color

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


-- tweakPolygonImage s sc pi@(PolygonImage source@(Image w h _) polys) = do
--   polys' <- mapM (maybeTweak 0.98 $ tweakPolygon s sc w h) polys
--             >>= maybeTweak 0.015 (addPolygon source)
--             >>= maybeTweak 0.0006 randomRemove
--             >>= reorderItems 0.0015

--   if polys' == polys
--   then tweakPolygonImage s sc pi
--   else return $ PolygonImage source polys'


-- tweakPolygon :: Float -> Float -> Int -> Int -> Polygon -> RVar Polygon
-- tweakPolygon s sc w h (Polygon points color) =
--   Polygon <$> points' <*> color'
--   where
--     color' = tweakColor 0.00075 sc color

--     points' =
--       mapM (maybeTweak 0.004 $ tweakPoint s) points
--       >>= maybeTweak 0.0015 (addItem $ genPoint w h)
--       >>= maybeTweak 0.0006 (randomRemoveRespectMin 3)


tweakPoint :: Float -> Point -> RVar Point
tweakPoint s (V2 x y) =
  V2 <$> tweak x <*> tweak y
  where
    tweak v = (+v) <$> normal 0 s


addPolygon :: Image a -> [Polygon] -> RVar [Polygon]
addPolygon (Image w h _) =
  liftM2 (:) (genPolygon w h) . return


polygonImageFitness :: PolygonImage -> Double
polygonImageFitness pi@(PolygonImage source _) =
  imageFitness source $ renderPolygonImage pi


-- TODO this duplication is awkward af
-- all of these should either wholesale replace the old versions or be done via newtype or similar
tweakPolygonImage' s sc pi@(PolygonImage source@(Image w h _) polys) = do
  polys' <- mapM (maybeTweak 0.98 $ tweakPolygon' s sc w h) polys
            >>= maybeTweak 0.001 (liftM2 (:) (genPolygon' w h) . return)
            >>= maybeTweak 0.001 randomRemove
            >>= reorderItems 0.00025

  if polys' == polys
  then tweakPolygonImage' s sc pi
  else return $ PolygonImage source polys'


genPolygon' :: Int -> Int -> RVar Polygon
genPolygon' w h = do
  start <- genPoint w h
  numPoints <- (+2) . abs <$> normalInt 0
  points <- replicateM numPoints (randomTranslate start)
  Polygon (start : points) <$> uniformColor

  where
    sx = fromIntegral w / 100
    sy = fromIntegral h / 100

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
  let halfway = (before + after) * (V2 0.5 0.5)
  return $ take index points ++ [halfway] ++ drop index points


tweakPolygon' s sc w h (Polygon points color) = do
  Polygon <$> points' <*> color'
  where
    color' = tweakColor 0.00075 sc color

    points' =
      maybeTweak 0.00015 splitLine points
      >>= mapM (maybeTweak 0.004 $ tweakPoint s)
      >>= maybeTweak 0.0006 (randomRemoveRespectMin 3)

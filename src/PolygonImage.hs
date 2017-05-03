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
  type Fitness PolygonImage = Down Double
  fitness = Down . polygonImageFitness

instance MonadRandom m => Tweakable m PolygonImage where
  type TweakConfig PolygonImage = Image PixelRGBA8
  generate = sample . polygonImageGen 10
  mutate _ = sample . tweakPolygonImage 2.5 16

instance MonadRandom m => Individual m PolygonImage

polygonImageGen :: Int -> Image PixelRGBA8 -> RVar PolygonImage
polygonImageGen polygonCount sourceImg@(Image w h _) =
  PolygonImage sourceImg <$> replicateM polygonCount (genPolygon w h)


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


tweakPolygonImage s sc pi@(PolygonImage source@(Image w h _) polys) = do
  polys' <- mapM (maybeTweak 0.98 $ tweakPolygon s sc w h) polys
            >>= maybeTweak 0.0015 (addPolygon source)
            >>= maybeTweak 0.0006 randomRemove
            >>= reorderItems 0.0015

  if polys' == polys
  then tweakPolygonImage s sc pi
  else return $ PolygonImage source polys'


tweakPolygon :: Float -> Float -> Int -> Int -> Polygon -> RVar Polygon
tweakPolygon s sc w h (Polygon points color) =
  Polygon <$> points' <*> color'
  where
    color' = tweakColor 0.00075 sc color

    points' =
      mapM (maybeTweak 0.002 $ tweakPoint s) points
      >>= maybeTweak 0.0006 (addItem $ genPoint w h)
      >>= maybeTweak 0.0006 (randomRemoveRespectMin 3)


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

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PolygonImage where

import Codec.Picture
import Control.DeepSeq
import Control.Monad
import Data.Random
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import GHC.Generics
import ImageUtils
import Util
import Evolution


data Polygon =
  Polygon [Point] PixelRGBA8
  deriving (Read, Show, Generic)


data PolygonImage =
  PolygonImage (Image PixelRGBA8) [Polygon]
  deriving (Generic)

instance NFData Polygon
instance NFData PolygonImage

instance Show PolygonImage where
  show (PolygonImage _ polygons) = show polygons


instance Individual RVar PolygonImage where
  fitness = polygonImageFitness
  recombine a b = return a
  mutate = tweakPolygonImage 2.5 16


polygonImageGen :: Int -> Image PixelRGBA8 -> RVar PolygonImage
polygonImageGen polygonCount sourceImg@(Image w h _) =
  PolygonImage sourceImg <$> replicateM polygonCount (genPolygon w h)


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


tweakPolygonImage s sc (PolygonImage source@(Image w h _) polys) =
  mapM (maybeTweak 0.15 $ tweakPolygon s sc w h) polys
  >>= maybeTweak 0.05 (addPolygon source)
  >>= maybeTweak 0.05 randomRemove
  >>= return . PolygonImage source


tweakPolygon :: Float -> Float -> Int -> Int -> Polygon -> RVar Polygon
tweakPolygon s sc w h (Polygon points color) =
  Polygon <$> points' <*> tweakPixelRGBA8 sc color

  where
    points' =
      mapM (maybeTweak 0.35 $ tweakPoint s) points
      >>= maybeTweak 0.05 (addItem $ genPoint w h)
      >>= maybeTweak 0.05 (randomRemoveRespectMin 3)


tweakPoint :: Float -> Point -> RVar Point
tweakPoint s (V2 x y) =
  V2 <$> tweak x <*> tweak y
  where
    tweak v = (+v) <$> normal 0 s

addPolygon :: Image a -> [Polygon] -> RVar [Polygon]
addPolygon (Image w h _) =
  liftM2 (:) (genPolygon w h) . return


polygonImageFitness pi@(PolygonImage source _) =
  imageFitness source $ renderPolygonImage pi

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module GLPolygonImage where

import           Codec.Picture          (Image (..), PixelRGBA8 (..))
import           Control.Monad.IO.Class
import           Data.Random            (MonadRandom, sample)
import qualified Graphics.Gloss         as GLS
import           Graphics.Rasterific    (V2 (..))

import           Evolution
import           GlossIndividual
import           PolygonImage


newtype GLPolygonImage = GLPolygonImage { getPolygonImage :: PolygonImage }


instance Show GLPolygonImage where
  showsPrec p = showsPrec p . getPolygonImage


instance AsGlossPicture Polygon where
  toPicture (Polygon points (PixelRGBA8 r g b a)) =
    GLS.color color $ GLS.polygon (map toPoint points)
    where
      color = GLS.makeColorI (fromEnum r) (fromEnum g) (fromEnum b) (fromEnum a)

      toPoint (V2 x y) = (x, y)

instance AsGlossPicture GLPolygonImage where
  toPicture (GLPolygonImage (PolygonImage _ polys)) =
    GLS.pictures (map toPicture polys)


instance MonadRandom m => Tweakable m GLPolygonImage where
  type TweakConfig GLPolygonImage = Image PixelRGBA8
  generate = fmap GLPolygonImage . generate
  mutate x = fmap GLPolygonImage . mutate x . getPolygonImage


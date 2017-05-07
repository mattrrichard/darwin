{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module GlossIndividual
  ( AsGlossPicture (..)
  , GlossIndividual
  , getRendered
  , RenderState
  , initGlossIndividual
  , initRenderState
  ) where

import           Codec.Picture                (Image (..), PixelRGBA8)
import           Control.DeepSeq
import           Control.Lens
import           Control.Lens.TH              (makeLenses)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader
import           Data.Ord                     (Down (..))
import qualified Data.Vector.Storable         as V
import           Foreign.ForeignPtr           (mallocForeignPtrArray,
                                               withForeignPtr)
import           Foreign.Ptr                  (nullPtr)
import           GHC.Word                     (Word32)
import qualified Graphics.Gloss               as GLS
import qualified Graphics.Gloss.Rendering     as GLSR
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL

import           Evolution
import           ImageUtils



class AsGlossPicture a where
  toPicture :: a -> GLS.Picture


data GlossIndividual a =
  GlossIndividual { _base        :: a
                  , _getRendered :: Image PixelRGBA8
                  , _getFitness  :: Down Word32
                  }

makeLenses ''GlossIndividual


-- TODO: Maybe this should serialize to something else?
-- Json? At least with Aeson, we'd be using Text instead of String
instance Show a => Show (GlossIndividual a) where
  showsPrec x = showsPrec x . view base


instance HasFitness (GlossIndividual a) where
  type Fitness (GlossIndividual a) = Down Word32
  fitness = view getFitness


data RenderState c =
  RenderState { _texture         :: GL.TextureObject
              , _imageFbo        :: GL.FramebufferObject
              , _multisampledFbo :: GL.FramebufferObject
              , _glossState      :: GLSR.State
              , _sourceImg       :: Image PixelRGBA8
              , _baseTweakConfig :: c
              }

makeLenses ''RenderState

instance (Tweakable m a, MonadIO m, AsGlossPicture a) => Tweakable m (GlossIndividual a) where
  type TweakConfig (GlossIndividual a) = RenderState (TweakConfig a)

  generate renderState =
    mkGlossIndividual renderState newBaseAction
    where newBaseAction = generate (renderState ^. baseTweakConfig)

  mutate renderState (view base -> oldBase) =
    mkGlossIndividual renderState newBaseAction
    where newBaseAction = mutate (renderState ^. baseTweakConfig) oldBase


pattern AsInt x <- (fromEnum -> x)
pattern AsEnum x <- (toEnum -> x)

imageSize (Image w h _) = (w, h)

initGlossIndividual :: (MonadIO m, AsGlossPicture a)
  => RenderState c -> a -> m (GlossIndividual a)
initGlossIndividual renderState =
  mkGlossIndividual renderState . return


mkGlossIndividual :: (MonadIO m, AsGlossPicture a)
  => RenderState c -> m a -> m (GlossIndividual a)
mkGlossIndividual renderState newBaseAction = do
  newBase <- newBaseAction
  rendered <- liftIO . renderToImage renderState . toPicture $ newBase

  let fit = imageFitness (renderState ^. sourceImg) rendered
  return $ GlossIndividual newBase rendered fit


initRenderState :: Image PixelRGBA8 -> c -> IO (RenderState c)
initRenderState sourceImg baseTweakConfig = do
  let (AsEnum w, AsEnum h) = imageSize sourceImg

  tex <- createTexture w h
  fbo <- GL.genObjectName
  renderBuffer <- GL.genObjectName

  GL.bindFramebuffer GL.Framebuffer $= fbo

  let samples = GL.Samples 8
  let size = GL.RenderbufferSize w h

  GL.bindRenderbuffer GL.Renderbuffer $= renderBuffer
  GL.renderbufferStorageMultiSample GL.Renderbuffer samples GL.RGBA' size
  GL.framebufferRenderbuffer GL.Framebuffer (GL.ColorAttachment 0) GL.Renderbuffer renderBuffer

  imageFbo <- GL.genObjectName
  GL.bindFramebuffer GL.Framebuffer $= imageFbo
  GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D tex 0

  GL.multisample $= GL.Enabled

  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  glossState <- GLSR.initState

  return RenderState { _texture = tex
                     , _imageFbo = imageFbo
                     , _multisampledFbo = fbo
                     , _glossState = glossState
                     , _sourceImg = sourceImg
                     , _baseTweakConfig = baseTweakConfig
                     }


createTexture :: GL.GLsizei -> GL.GLsizei -> IO GL.TextureObject
createTexture w h = do
  let pixelData = GL.PixelData GL.RGBA GL.UnsignedByte nullPtr
  let size = GL.TextureSize2D w h

  tex <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just tex
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB' size 0 pixelData

  return tex


readToImage :: GL.Size -> IO (Image PixelRGBA8)
readToImage size@(GL.Size (AsInt w) (AsInt h)) = do
  let bytes = w * h * 4

  memory <- mallocForeignPtrArray bytes
  withForeignPtr memory $ \ptr -> do
    let pixelData = GL.PixelData GL.RGBA GL.UnsignedByte ptr
    GL.readPixels (GL.Position 0 0) size pixelData

  let vector = V.unsafeFromForeignPtr0 memory bytes
  return (Image w h vector)


white = GLSR.makeColor 1 1 1 1


renderToImage :: RenderState c -> GLS.Picture -> IO (Image PixelRGBA8)
renderToImage renderState pic = do
  let (w, h) = renderState ^. sourceImg . to imageSize
  let (w', h') = (toEnum w, toEnum h)

  let fbo = renderState ^. multisampledFbo
  let imageBuffer = renderState ^. imageFbo

  let bottomLeft = GL.Position 0 0
  let size = GL.Size w' h'
  let topRight = GL.Position w' h'

  -- bind multisampled buffer
  GL.bindFramebuffer GL.Framebuffer $= fbo

  -- draw image
  GL.viewport $= (bottomLeft, size)
  -- gloss default puts (0,0) in the center of the screen.  Move it to bottom left
  let recenteredPic = GLS.translate (-fromIntegral w / 2) (-fromIntegral h / 2) pic
  GLSR.displayPicture (w, h) white (renderState ^. glossState) 1 recenteredPic

  -- setup buffers to blit (read from multisampled, write to buffer with texture bound)
  GL.bindFramebuffer GL.ReadFramebuffer $= fbo
  GL.bindFramebuffer GL.DrawFramebuffer $= imageBuffer
  GL.blitFramebuffer bottomLeft topRight bottomLeft topRight [GL.ColorBuffer'] GL.Nearest

  -- bind texture buffer before reading it out to an image
  GL.bindFramebuffer GL.ReadFramebuffer $= imageBuffer
  image <- readToImage size

  -- reset framebuffer TODO: do you actually need this?
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

  return image



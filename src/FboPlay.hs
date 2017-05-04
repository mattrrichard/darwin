{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module FboPlay where

import qualified Graphics.Gloss               as GLS
import qualified Graphics.Gloss.Rendering     as GR
import qualified Graphics.Rendering.OpenGL    as GL
import           Graphics.Rendering.OpenGL.GL (($=))

import qualified Graphics.UI.GLUT             as G

import qualified Codec.Picture                as P
import           Data.Proxy
import qualified Data.Vector.Storable         as V
import           Foreign.ForeignPtr           (mallocForeignPtrArray,
                                               withForeignPtr)
import           Foreign.Ptr                  (nullPtr)
import           Graphics.Rasterific          (V2(..))

import           Data.Bifunctor
import Data.Ord (Down(..))
import Data.Random

import Control.Monad.IO.Class

import           PolygonImage
import Evolution
import ImageUtils


createTexture w h = do
  let pixelData = GL.PixelData GL.RGBA GL.UnsignedByte nullPtr
  let size = GL.TextureSize2D w h

  tex <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just tex
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB' size 0 pixelData

  return tex

createMultisampleTexture w h  = do
  let size = GL.TextureSize2D w h

  tex <- GL.genObjectName

  GL.textureBinding GL.Texture2DMultisample $= Just tex
  GL.texImage2DMultisample GL.Texture2DMultisample GL.NoProxy (GL.Samples 8) GL.RGBA' size GL.FlexibleSampleLocations

  return tex


renderToImage w h (tex, fbo, renderBuffer, imageFbo, state) renderAction = do
  GL.bindFramebuffer GL.Framebuffer $= fbo
  renderAction
  G.flush
  GL.bindFramebuffer GL.ReadFramebuffer $= fbo
  GL.bindFramebuffer GL.DrawFramebuffer $= imageFbo
  let bottomLeft = GL.Position 0 0
  let topRight = GL.Position (toEnum w) (toEnum h)

  GL.blitFramebuffer bottomLeft topRight bottomLeft topRight [GL.ColorBuffer'] GL.Nearest
  GL.bindFramebuffer GL.ReadFramebuffer $= imageFbo

  image <- readToImage w h

  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

  return image


readToImage :: Int -> Int -> IO (P.Image P.PixelRGBA8)
readToImage w h = do
  let bytes = w*h*4
  memory <- mallocForeignPtrArray bytes
  withForeignPtr memory $ \ptr -> do
    let pixelData = GL.PixelData GL.RGBA GL.UnsignedByte ptr
    GL.readPixels (GL.Position 0 0) (GL.Size (toEnum w) (toEnum h)) pixelData

  let vector = V.unsafeFromForeignPtr0 memory bytes
  return (P.Image w h vector)


polygonImageToPicture :: PolygonImage -> GLS.Picture
polygonImageToPicture (PolygonImage (P.Image w h _) polys) =
  GLS.translate (negate $ fromIntegral w / 2) (negate $ fromIntegral h / 2) $
  GLS.pictures $ map toImage polys
  where
    toImage (Polygon points (P.PixelRGBA8 r g b a)) =
      GLS.color (GLS.makeColorI (fromEnum r) (fromEnum g) (fromEnum b) (fromEnum a))
      $ GLS.polygon (map toPoint points)

    toPoint (V2 x y) = (x, y)


data PolygonPicture =
  PolygonPicture { polys :: PolygonImage
                 , image :: P.Image P.PixelRGBA8
                 }

type RenderState = (GL.TextureObject, GL.FramebufferObject, GL.RenderbufferObject, GL.FramebufferObject, GR.State)

initRenderState :: Int -> Int -> IO RenderState
initRenderState w h = do
  tex <- createTexture (toEnum w) (toEnum h)
  fbo <- GL.genObjectName

  renderBuffer <- GL.genObjectName

  GL.bindFramebuffer GL.Framebuffer $= fbo

  GL.bindRenderbuffer GL.Renderbuffer $= renderBuffer
  GL.renderbufferStorageMultiSample GL.Renderbuffer (GL.Samples 8) GL.RGBA' (GL.RenderbufferSize (toEnum w) (toEnum h))
  GL.framebufferRenderbuffer GL.Framebuffer (GL.ColorAttachment 0) GL.Renderbuffer renderBuffer

  imageFbo <- GL.genObjectName
  GL.bindFramebuffer GL.Framebuffer $= imageFbo
  GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D tex 0

  GL.multisample $= GL.Enabled

  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  glossState <- GR.initState


  return (tex, fbo, renderBuffer, imageFbo, glossState)

instance Show PolygonPicture where
  show (PolygonPicture pi _) = show pi

instance HasFitness PolygonPicture where
  type Fitness PolygonPicture = Down Double
  fitness (PolygonPicture (PolygonImage source _) rendered) = Down $ imageFitness source rendered

instance (MonadRandom m, MonadIO m) => Tweakable m PolygonPicture where
  type TweakConfig PolygonPicture = RenderState
  generate = undefined
  mutate renderState (PolygonPicture pi _) = do
    newImage <- mutate undefined pi --TODO
    rendered <- liftIO $ renderPolygonPicture renderState newImage
    return (PolygonPicture newImage rendered)


instance (MonadRandom m, MonadIO m) => Individual m PolygonPicture

renderPP renderState (PolygonPicture pi _) = renderPolygonPicture renderState pi

renderPolygonPicture :: RenderState -> PolygonImage -> IO (P.Image P.PixelRGBA8)
renderPolygonPicture renderState pi@(PolygonImage (P.Image w h _) _) = do
  let pic = polygonImageToPicture pi
  renderToImage w h renderState $ do
    GL.viewport $= (GL.Position 0 0, GL.Size (toEnum w) (toEnum h))
    state <- GR.initState --TODO can't just create these every time we render lol
    GR.displayPicture (w, h) (GR.makeColor 1 1 1 1) state 1 pic


renderToPolygonPicture :: RenderState -> PolygonImage -> IO PolygonPicture
renderToPolygonPicture renderState pi =
  PolygonPicture pi <$> renderPolygonPicture renderState pi


picture =
  GLS.pictures [ t 0 0 $ c GLS.red
               , t 15 15 $ c GLS.azure
               , t (-20) (-20) $ c GLS.green
               ]
  where
    t = GLS.translate
    c c = GLS.Color (GLS.withAlpha 0.8 c) $ GLS.circleSolid 20

circle :: GLS.Picture
circle = GLS.circleSolid 20


stuff :: IO ()
stuff = do
  G.initialize "darwin" []
  G.initialWindowSize $= G.Size 320 240
  -- G.initialWindowSize $= G.Size 100 100

  window <- G.createWindow "darwin"

  let textureSize = (100, 100) :: (Int, Int)

  state <- GR.initState

  tex <- uncurry createTexture (bimap toEnum toEnum textureSize)
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Clamp)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Clamp)

  fbo <- GL.genObjectName :: IO GL.FramebufferObject
  GL.bindFramebuffer GL.Framebuffer $= fbo
  GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D tex 0

  GL.textureBinding GL.Texture2D $= Nothing
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

  G.displayCallback $= do
    GL.viewport $= (GL.Position 0 0, uncurry GL.Size $ bimap toEnum toEnum textureSize)
    GR.withModelview textureSize $ do
      GL.bindFramebuffer GL.Framebuffer $= fbo
      GL.textureBinding GL.Texture2D $= Nothing
      GL.texture GL.Texture2D $= GL.Disabled

      GL.clearColor $= GL.Color4 0 0 0 (1 :: GL.GLfloat)
      GL.clear [GL.ColorBuffer]

      GL.clearColor $= GL.Color4 0.5 0.4 0.7 (1 :: GL.GLfloat)
      GL.clear [GL.ColorBuffer]

      GR.renderPicture state 2 picture

      image <- uncurry readToImage textureSize
      P.writePng "buffer.png" image

      GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

    G.Size w h <- GL.get G.windowSize

    GR.withModelview (fromEnum w, fromEnum h) $ do
      GL.viewport $= (GL.Position 0 0, GL.Size w h)
      GL.clearColor $= GL.Color4 0 0 0 (1 :: GL.GLfloat)
      GL.clear [GL.ColorBuffer]

      GL.texture GL.Texture2D $= GL.Enabled
      GL.activeTexture $= GL.TextureUnit 0
      GL.textureBinding GL.Texture2D $= Just tex

      GL.textureGenMode GL.S $= Just GL.SphereMap
      GL.textureGenMode GL.T $= Just GL.SphereMap

      GL.preservingMatrix $ do
        G.rotate 30 (G.Vector3 0 1 (1::GL.GLfloat))
        cube 100
    G.flush


  G.mainLoop

  return ()

-- stuff' = do
--   G.initialize "darwin" []

--   window <- G.createWindow "darwin"

--   state <- GR.initState

--   image <- renderToImage 320 240 $ do
--     GL.viewport $= (GL.Position 0 0 , GL.Size 320 240)
--     GR.withModelview (320, 240) $ do
--       GL.clearColor $= GL.Color4 0 0 0 (1 :: GL.GLfloat)
--       GL.clear [GL.ColorBuffer]
--       GR.renderPicture state 2 circle
--       GL.flush

--   P.writePng "buffer.png" image

cube :: G.Height -> IO ()
cube =
  G.renderObject G.Solid . G.Cube

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where
import           Codec.Picture
import           Control.Concurrent           (forkOS)
import           Control.Monad
import           Control.Monad.Extra
import qualified Data.Char                    as C (toLower)
import           Data.IORef
import           Data.List
import           Data.Random
import qualified Data.Vector.Storable         as V
import           Evolution
import           Graphics.Rasterific
import           ImageUtils
import           Pipes
import qualified Pipes.Prelude                as P
import           PolygonImage
import           Strategies
import           System.Environment
import qualified System.Exit                  as S
import           System.IO

-- import qualified Graphics.Gloss as G
import qualified FboPlay                      as F
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT             as G


data Config s a b c =
  Config { strategy      ::  s
         , indGen        :: IO a
         , readPop       :: b -> IO a
         , runName       :: String
         , startingGenId :: Int
         , render        :: a -> IO (Image PixelRGBA8)
         , stepCount     :: Int
         , tweakConfig   :: c
         }


runner :: (EvolutionStrategy s, Individual IO a, Show a, Read b, Show (Fitness a))
  => Config s a b (TweakConfig a)
  -> IO ()
runner Config {..} = do

  putStrLn "loading initial"
  startingGen <- loadGen startingGenId
  -- readble <- read <$> readFile (fileName startingGenId ++ ".data")
  -- startingGen <- readPop (readble !! 22)

  putStrLn $ "starting at gen " ++ show startingGenId
  runEffect $ for (pipeline startingGen) processGeneration

  where
    pipeline startingGen =
      P.zip (evolve strategy tweakConfig startingGen) (each [startingGenId+1..])
      >-> P.chain (print . snd)
      >-> P.chain (const $ hFlush stdout)
      -- >-> P.chain (print . fst)
      >-> pipeSkip stepCount

    fileName id = "out/" ++ runName ++ show id

    loadGen 0 = replicateM (popSize strategy) indGen
    loadGen id =
      mapM readPop . read =<< readFile (fileName id ++ ".data")

    processGeneration (gen, genId) =
      lift $ do
        putStrLn ("completed gen " ++ show genId)
        rendered <- render (head gen)
        writePng (fileName genId ++ ".png") rendered
        writeFile (fileName genId ++ ".data") $ show gen

    -- The fact that I had to write this myself feels wrong.  Did I miss something?
    pipeSkip n = forever $ do
      skip (n-1)
      await >>= yield
      where skip 0 = return ()
            skip n = await >> skip (n-1)


polygonConfig sourceImg s startingGen stepCount =
  return Config { strategy = s
                , indGen = return $ initEmpty sourceImg
                , readPop = return . PolygonImage sourceImg
                , runName = "polygons"
                , startingGenId = startingGen
                , render = return . renderPolygonImage
                , stepCount = stepCount
                , tweakConfig = sourceImg
                }

glConfig sourceImg@(Image w h _) s startingGen stepCount = do
  G.initialize "darwin" []
  window <- G.createWindow "darwin"
  G.displayCallback $= return ()

  renderState <- F.initRenderState w h

  return Config { strategy = s
                , indGen = F.renderToPolygonPicture renderState $ initEmpty sourceImg
                , readPop = F.renderToPolygonPicture renderState . PolygonImage sourceImg
                , runName = "polygons"
                , startingGenId = startingGen
                , render = F.renderPP renderState
                , stepCount = stepCount
                , tweakConfig = renderState
                }

-- circleConfig sourceImg s startingGen stepCount =
--   Config { strategy = s
--          , indGen = sample $ circleImageGen 50 sourceImg
--          , readPop = CircleImage sourceImg
--          , runName = "circles"
--          , startingGenId = startingGen
--          , render = renderCircleImage
--          , stepCount = stepCount
--          }


main :: IO ()
main = do
  [filename] <- getArgs
  (Right source) <- readImage filename --"images/landscape-st-remy-306-240.jpg"
  let sourceImg = convertRGBA8 source

  forkOS $ do
    config <- glConfig sourceImg (MuPlusLambda 1 1) 0 500
    -- config <- polygonConfig sourceImg (MuPlusLambda 1 1) 6000 25
    runner config

  forever $ do
    hSetBuffering stdin NoBuffering
    c <- getChar
    when (C.toLower c == 'q') S.exitSuccess
    -- when (C.toLower c == 'q') $ writeIORef quitSignal True

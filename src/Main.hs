{-# LANGUAGE RecordWildCards      #-}

module Main where

import           CircleImage
import           Codec.Picture
import           Control.Monad
import           Control.Monad.Extra
import           Data.List
import           Data.Random
import qualified Data.Vector.Storable as V
import           Evolution
import           Graphics.Rasterific
import           ImageUtils
import           Pipes
import qualified Pipes.Prelude        as P
import           PolygonImage
import           Strategies
import           System.Environment
import           System.IO


data Config s a b =
  Config { strategy      ::  s
         , indGen        :: IO a
         , readPop       :: b -> a
         , runName       :: String
         , startingGenId :: Int
         , render        :: a -> Image PixelRGBA8
         , stepCount     :: Int
         }


runner :: (EvolutionStrategy s, Individual a, Show a, Read b) => Config s a b -> IO ()
runner Config {..} = do
  startingGen <- loadGen startingGenId
  runEffect $
    P.zip (evolve strategy startingGen) (each [startingGenId..])
    >-> pipeSkip stepCount
    >-> processGeneration

  where
    fileName id = "out/" ++ runName ++ show id

    loadGen 0 = replicateM (popSize strategy) indGen
    loadGen id =
      map readPop . read <$> readFile (fileName id ++ ".data")

    processGeneration = forever $ do
      (gen, genId) <- await
      lift $ do
        putStrLn ("completed gen " ++ show genId)
        writePng (fileName genId ++ ".png") $ render (head gen)
        writeFile (fileName genId ++ ".data") $ show gen



polygonConfig sourceImg s startingGen stepCount =
  Config { strategy = s
         , indGen = return $ initEmpty sourceImg
         , readPop = PolygonImage sourceImg
         , runName = "polygons"
         , startingGenId = startingGen
         , render = renderPolygonImage
         , stepCount = stepCount
         }

circleConfig sourceImg s startingGen stepCount =
  Config { strategy = s
         , indGen = sample $ circleImageGen 50 sourceImg
         , readPop = CircleImage sourceImg
         , runName = "circles"
         , startingGenId = startingGen
         , render = renderCircleImage
         , stepCount = stepCount
         }


main :: IO ()
main = do
  [filename] <- getArgs
  (Right source) <- readImage filename --"images/landscape-st-remy-306-240.jpg"
  let sourceImg = convertRGBA8 source

  let config = polygonConfig sourceImg (MuPlusLambda 4 32) 0 25

  runner config

-- the fact that I had to make these two functions myself feels wrong.  Maybe I missed something?
pipeSkip n = forever $ do
  await >>= yield
  replicateM_ (n-1) await


pipeWithIndex :: Monad m => Pipe a (a, Int) m r
pipeWithIndex = do
  first <- await
  P.scan step (first, 0) id

  where step (_, index) x = (x, index + 1)

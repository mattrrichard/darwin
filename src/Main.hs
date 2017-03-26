{-# LANGUAGE RecordWildCards #-}

module Main where
import           CircleImage
import           Codec.Picture
import           Control.Concurrent   (forkIO)
import           Control.Monad
import           Control.Monad.Extra
import qualified Data.Char            as C (toLower)
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
import qualified System.Exit          as S
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
  putStrLn $ "starting at gen " ++ show startingGenId
  runEffect $ for (pipeline startingGen) processGeneration

  where
    pipeline startingGen =
      P.zip (evolve strategy startingGen) (each [startingGenId+1..])
      >-> P.chain (print . snd)
      >-> pipeSkip stepCount

    fileName id = "out/" ++ runName ++ show id

    loadGen 0 = replicateM (popSize strategy) indGen
    loadGen id =
      map readPop . read <$> readFile (fileName id ++ ".data")

    processGeneration (gen, genId) =
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

  let config = polygonConfig sourceImg (MuPlusLambda 4 32) 6300 25

  forkIO $ runner config

  forever $ do
    hSetBuffering stdin NoBuffering
    c <- getChar
    when (C.toLower c == 'q') S.exitSuccess

-- The fact that I had to write this myself feels wrong.  Did I miss something?
pipeSkip n = forever $ do
  skip (n-1)
  await >>= yield
  where skip 0 = return ()
        skip n = await >> skip (n-1)

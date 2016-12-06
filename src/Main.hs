{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import           PolygonImage
import           SimulatedAnnealing
import           Strategies
import           System.IO
import           System.Environment


data Config s a b =
  Config { strategy ::  s
         , indGen :: RVar a
         , readPop :: b -> a
         , runName :: String
         , startingGenId :: Int
         , render :: a -> Image PixelRGBA8
         , stepCount :: Int
         }


runner :: (EvolutionStrategy s, Individual RVar a, Show a, Read b) => Config s a b -> IO ()
runner (Config strategy indGen readPop runName startingGenId render stepCount) = do
  startingGen <- loadGen startingGenId

  void $ iterateM loop (startingGenId, startingGen)

  where
    fileName id = "out/" ++ runName ++ show id

    loadGen 0 = sample $ replicateM (popSize strategy) indGen
    loadGen id =
      map readPop . read <$> readFile (fileName id ++ ".data")

    loop (genId, population) = do
      putStrLn $ "starting from generation " ++ show genId
      hFlush stdout

      lastGen <- runRVar (last . take stepCount $ evolve strategy population) StdRandom

      let best = head lastGen
      let endingGenId = genId + stepCount

      writePng (fileName endingGenId ++ ".png") $ render best

      writeFile (fileName endingGenId ++ ".data") $ show lastGen

      return (endingGenId, lastGen)


polygonConfig sourceImg s startingGen stepCount =
  Config { strategy = s
         , indGen = polygonImageGen 25 sourceImg
         , readPop = PolygonImage sourceImg
         , runName = "polygons"
         , startingGenId = startingGen
         , render = renderPolygonImage
         , stepCount = stepCount
         }

circleConfig sourceImg s startingGen stepCount =
  Config { strategy = s
         , indGen = circleImageGen 50 sourceImg
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

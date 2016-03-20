{-# LANGUAGE FlexibleContexts #-}
module Main where

import Codec.Picture

main :: IO ()
main = do
  i <- readImage "images/1280-starry-night.jpg"
  case i of
    Left err -> print err
    Right img -> go $ convertRGB8 img

  where
    go (Image w h ps) = print ps


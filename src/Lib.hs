{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Text.HandsomeSoup
import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy as BS

someFunc :: IO ()
someFunc = do
  r <- get "http://wikipedia.org"
  BS.putStrLn $ (r ^. responseBody)

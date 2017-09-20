{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import           Data.Aeson                 (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

someFunc :: IO ()
someFunc = do
    response <- httpJSON "http://httpbin.org/get"
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

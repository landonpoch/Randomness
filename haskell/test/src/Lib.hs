{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( bootstrap
    ) where

import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

bootstrap :: String -> IO ()
bootstrap val = do
    request <- parseRequest val
    response <- httpJSON request
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

-- someFunc :: IO ()
-- someFunc = do
--     response <- httpJSON "https://webapp.movetv.com/npv/cfdir.json"
--     S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

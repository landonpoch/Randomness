{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( bootstrap
    ) where

import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Control.Monad
import           Network.HTTP.Simple

bootstrap :: String -> IO ()
bootstrap = S8.putStrLn <=< fmap encodeResponse . httpJSON <=< parseRequest
    -- response <- httpJSON <=< parseRequest $ val
    -- S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

encodeResponse :: Response Value -> S8.ByteString
encodeResponse = Yaml.encode . getResponseBody

-- someFunc :: IO ()
-- someFunc = do
--     response <- httpJSON "https://webapp.movetv.com/npv/cfdir.json"
--     S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

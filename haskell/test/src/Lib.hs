{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
    ( test
    ) where

import           Data.Aeson            
import           Data.Aeson.Types      as A
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Control.Monad
import           Network.HTTP.Simple
import           Data.HashMap.Strict
-- import           Text.Read             (readMaybe)

bootstrap :: String -> IO ()
bootstrap = S8.putStrLn <=< fmap encodeResponse . httpJSON <=< parseRequest

encodeResponse :: Response Value -> S8.ByteString
encodeResponse = Yaml.encode . getResponseBody

test :: String -> IO (Environments)
test url = do
    request <- (parseRequest url)
    response <- (httpJSON :: Request -> IO (Response Environments)) request
    let responseBody = getResponseBody response
    return responseBody

data Environment = Environment
    { configHost    :: !String
    , configHostSsl :: Maybe String
    } deriving Show

instance FromJSON Environment where
    parseJSON = withObject "environment" $ \o -> do
        configHost    <- o .: "config_host"
        configHostSsl <- o .:? "config_host_ssl"
        return Environment{..}

data Environments = Environments { environments :: HashMap String Environment } deriving Show

instance FromJSON Environments where
    parseJSON = withObject "environments" $ \o -> do
        environments <- o .: "environments"
        return Environments{..}

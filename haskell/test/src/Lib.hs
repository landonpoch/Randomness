{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
    ( test
    ) where

import           Data.Aeson            
import           Data.Aeson.Types      as A
import qualified Data.ByteString.Char8 as S8
-- import qualified Data.Yaml             as Yaml
import           Control.Monad
import           Network.HTTP.Simple

-- bootstrap :: String -> IO ()
-- bootstrap = S8.putStrLn <=< fmap encodeResponse . httpJSON <=< parseRequest

-- encodeResponse :: Response Value -> S8.ByteString
-- encodeResponse = Yaml.encode . getResponseBody

test :: Response Value -> IO ()
test response = do
    let value = getResponseBody response
    let json = (parseJSON :: Value -> Parser Environment) $ value
    let tester = eitherDecode value :: Either String Environment
    return ()

data Environment = Environment
    { configHost    :: !String
    , configHostSsl :: !String
    } deriving Show

instance FromJSON Environment where
    parseJSON = withObject "environment" $ \o -> do
        configHost    <- o .: "config_host"
        configHostSsl <- o .: "config_host_ssl"
        return Environment{..}
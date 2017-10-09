{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Environments
    ( Environments(..)
    , Environment(..)
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Aeson          (FromJSON, parseJSON, withObject, (.:), (.:?))

data Environment = Environment
    { configHost    :: !String
    , configHostSsl :: Maybe String
    } deriving Show

data Environments = Environments { environments :: HashMap String Environment } deriving Show

instance FromJSON Environment where
    parseJSON = withObject "environment" $ \o -> do
        configHost    <- o .: "config_host"
        configHostSsl <- o .:? "config_host_ssl"
        return Environment{..}

instance FromJSON Environments where
    parseJSON = withObject "environments" $ \o -> do
        environments <- o .: "environments"
        return Environments{..}
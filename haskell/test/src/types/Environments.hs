{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types.Environments
  ( Environments(..)
  , Environment(..)
  )
where

import           Protolude
import           Data.Aeson
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.Text                     as T

data Environment = Environment
    { configHost    :: !T.Text
    , configHostSsl :: Maybe T.Text
    } deriving Show

newtype Environments = Environments { environments :: HashMap T.Text Environment } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "environment" $ \o -> do
    configHost    <- o .: "config_host"
    configHostSsl <- o .:? "config_host_ssl"
    return Environment { .. }

instance ToJSON Environment where
  toJSON Environment {..} =
    object ["config_host" .= configHost, "config_host_ssl" .= configHostSsl]

instance FromJSON Environments where
  parseJSON = withObject "environments" $ \o -> do
    environments <- o .: "environments"
    return Environments { .. }

instance ToJSON Environments where
  toJSON Environments {..} = object ["environments" .= environments]

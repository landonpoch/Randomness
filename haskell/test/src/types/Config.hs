{-# LANGUAGE OverloadedStrings #-}

module Types.Config
  ( AppConfig(..)
  , Config(..)
  , UserConfig(..)
  , parseConfig
  )
where

import           Protolude
import           Control.Exception              ( throw )
-- TODO: is this fail thing really the right thing to use?
import           Control.Monad                  ( fail )
import qualified Data.Text                     as T
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                )
import qualified Data.Yaml                     as Y
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( Config(..)
                                                , LogLevel(..)
                                                , AppConfig(..)
                                                , UserConfig(..)
                                                )

instance FromJSON Config where
  parseJSON (Y.Object v) = Config <$> v .: "app" <*> v .: "user"
  parseJSON _            = fail "Unable to parse config"

instance FromJSON AppConfig where
  parseJSON (Y.Object v) =
    AppConfig
      <$> v
      .:  "root-url"
      <*> v
      .:  "environment"
      <*> v
      .:  "platform"
      <*> v
      .:  "pe-key"
      <*> v
      .:  "log-level"
  parseJSON _ = fail "Unable to parse app config"

instance FromJSON LogLevel where
  parseJSON (Y.String s) = parseVal s
   where
    parseVal "debug" = return Debug
    parseVal "info"  = return Info
    parseVal "warn"  = return Warn
    parseVal "error" = return Error
    parseVal "fatal" = return Fatal
    parseVal _       = fail "Invalid value for log-level in config file"
  parseJSON _ = fail "Invalid type for log-level in config file"

instance FromJSON UserConfig where
  parseJSON (Y.Object v) =
    UserConfig <$> v .: "email" <*> v .: "password" <*> v .: "device-guid"
  parseJSON _ = fail "Unable to parse user config"

-- parseConfig :: (MonadFile m, MonadLogger m) => m Config
parseConfig :: IO Config
parseConfig = do
  configText <- readFile "config.yaml"
  case Y.decodeEither' $ toS configText of
    Left  err    -> throw err
    Right config -> return config

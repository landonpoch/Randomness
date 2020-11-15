{-# LANGUAGE OverloadedStrings #-}

module Types.Config
  ( AppConfig(..)
  , Config(..)
  , UserConfig(..)
  , parseConfig
  )
where

import Protolude
    ( ($),
      Monad(return),
      Applicative((<*>)),
      Either(Right, Left),
      Text,
      (<$>),
      toS,
      readFile,
      MonadIO(..),
      Print(putStrLn) )
import           Control.Exception              ( throw )
-- TODO: is this fail thing really the right thing to use?
import           Control.Monad                  ( fail )
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                )
import qualified Data.Yaml                     as Y
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

parseConfig :: MonadIO m => m Config
parseConfig = do
  configText <- liftIO $ readFile "config.yaml"
  case Y.decodeEither' $ toS configText of
    Left  err    -> throw err
    Right config -> do
      putStrLn ("Using Config:" :: Text)
      putStrLn configText
      return config

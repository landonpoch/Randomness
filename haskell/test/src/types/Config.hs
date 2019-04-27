{-# LANGUAGE OverloadedStrings #-}

module Types.Config
  ( AppConfig(..)
  , Config(..)
  , UserConfig(..)
  , parseConfig
  )
where

import           Control.Exception              ( throw )
import           Control.Monad.Catch            ( MonadThrow )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                )
import qualified Data.Yaml                     as Y
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( MonadFile
                                                , MonadLogger
                                                , readFile'
                                                , trace
                                                )

data Config = Config
  { appConfig  :: AppConfig
  , userConfig :: UserConfig
  } deriving (Eq, Show)

data AppConfig = AppConfig
  { rootUrl     :: T.Text
  , environment :: T.Text
  , platform    :: T.Text
  , peKey       :: T.Text
  } deriving (Eq, Show)

data UserConfig = UserConfig
  { email      :: T.Text
  , password   :: T.Text
  , deviceGuid :: T.Text
  } deriving (Eq, Show)

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
  parseJSON _ = fail "Unable to parse app config"

instance FromJSON UserConfig where
  parseJSON (Y.Object v) =
    UserConfig <$> v .: "email" <*> v .: "password" <*> v .: "device-guid"
  parseJSON _ = fail "Unable to parse user config"

parseConfig :: (MonadFile m, MonadLogger m, MonadThrow m) => m Config
parseConfig = do
  configText <- readFile' "config.yaml"
  case Y.decodeEither $ TE.encodeUtf8 configText of
    Left  err    -> throw . YamlParseError $ T.pack err
    Right config -> do
      trace "Using config:"
      trace configText
      return config

{-# LANGUAGE OverloadedStrings #-}

module App.Bootstrapper
  ( bootstrap
  ) where

import           Control.Exception    (throw)
import           Control.Monad.Catch  (MonadThrow)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HMS
import qualified Data.Text            as T
import           Text.Printf          (printf)
import           Types.Config         (Config (..), environment, platform,
                                       rootUrl)
import qualified Types.Environments   as T
import           Types.Exceptions     (CustomException (..))
import           Types.Global         (MonadHttp, MonadLogger, Url)
import qualified Types.Hostnames      as TH
import           Utils.Fetch          (jsonRequest, request)

bootstrap :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => Config -> m BL.ByteString
bootstrap config = do
  let targetEnvironment = environment config
  let targetPlatform = platform config
  environments <- getEnvironments $ rootUrl config
  selectedEnv <- selectEnvironment environments targetEnvironment
  hostnamesByEnvironment <- getHostnames (T.configHost selectedEnv) targetPlatform
  selectedHostnames <- selectHostnames hostnamesByEnvironment targetEnvironment
  configHostname <- getConfigHost selectedHostnames
  -- throw RandomException
  getPeFile configHostname targetPlatform targetEnvironment
  -- TODO: Parse PE file

getEnvironments :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> m T.Environments
getEnvironments rootUrl = jsonRequest . T.pack $ printf "GET %s" rootUrl

getHostnames :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> T.Text -> m TH.HostnameEnvironments
getHostnames configHostname platform = do
  let hostnamesUrl = printf "GET %s/env-list/%s-sling.json" configHostname platform
  jsonRequest $ T.pack hostnamesUrl

getPeFile :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> T.Text -> T.Text -> m BL.ByteString
getPeFile configHost platform env = do
  let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" configHost platform env
  request $ T.pack peUrl

selectEnvironment :: (MonadThrow m) => T.Environments -> T.Text -> m T.Environment
selectEnvironment environments env = do
  let maybeEnvironment = HMS.lookup env $ T.environments environments
  convertMaybe maybeEnvironment "target environment doesn't exist"

selectHostnames :: (MonadThrow m) => TH.HostnameEnvironments -> T.Text -> m TH.Hostnames
selectHostnames hostnamesByEnvironment env = do
  let maybeHostnames = HMS.lookup env $ TH.environments hostnamesByEnvironment
  convertMaybe maybeHostnames "environment missing hostnames"

getConfigHost :: (MonadThrow m) => TH.Hostnames -> m T.Text
getConfigHost selectedHostnames = do
  let maybeConfigUrl = TH.appCastUrl selectedHostnames
  convertMaybe maybeConfigUrl "config url is missing from hostnames"

convertMaybe :: (MonadThrow m) => Maybe a -> T.Text -> m a
convertMaybe val msg = case val of
  Nothing  -> throw $ KeyNotFoundError msg
  (Just x) -> return x

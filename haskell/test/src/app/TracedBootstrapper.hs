module App.TracedBootstrapper
  ( bootstrap
  ) where

import           Control.Exception             (Exception, throw)
import qualified Data.ByteString.Lazy.Internal as I
import qualified Data.HashMap.Strict           as HMS (lookup)
import           Data.Typeable                 (Typeable)
import           Text.Printf                   (printf)
import           Types.Config                  (Config (Config), environment,
                                                platform, rootUrl)
import qualified Types.Environments            as T
import           Types.Exceptions              (CustomException (KeyNotFoundError))
import           Types.Global                  (MonadThrowHttp,
                                                MonadWriteThrowHttp)
import qualified Types.Hostnames               as TH
import qualified Utils.TracedFetch             as TF (jsonRequest, request)

data MyException = ThisException String | ThatException
  deriving (Show, Typeable)

instance Exception MyException

bootstrap :: (MonadWriteThrowHttp m) => Config -> m I.ByteString
bootstrap config = do
  let targetEnvironment = environment config
  let targetPlatform = platform config
  environments <- getEnvironments $ rootUrl config
  selectedEnv <- selectEnvironment environments targetEnvironment
  hostnamesByEnvironment <- getHostnames (T.configHost selectedEnv) targetPlatform
  selectedHostnames <- selectHostnames hostnamesByEnvironment targetEnvironment
  configHostname <- getConfigHost selectedHostnames
  -- TODO: If this exception occurs, nothing is added to the writer at all
  -- throw $ ThisException "Error happened!"
  getPeFile configHostname targetPlatform targetEnvironment
  -- TODO: Parse PE file

getEnvironments :: (MonadWriteThrowHttp m) => String -> m T.Environments
getEnvironments rootUrl = TF.jsonRequest $ printf "GET %s" rootUrl

selectEnvironment :: (MonadThrowHttp m) => T.Environments -> String -> m T.Environment
selectEnvironment environments env = do
  let maybeEnvironment = HMS.lookup env $ T.environments environments
  convertMaybe maybeEnvironment "target environment doesn't exist"

getHostnames :: (MonadWriteThrowHttp m) => String -> String -> m TH.HostnameEnvironments
getHostnames configHostname platform = do
  let hostnamesUrl = printf "GET %s/env-list/%s-sling.json" configHostname platform
  TF.jsonRequest hostnamesUrl

selectHostnames :: (MonadThrowHttp m) => TH.HostnameEnvironments -> String -> m TH.Hostnames
selectHostnames hostnamesByEnvironment env = do
  let maybeHostnames = HMS.lookup env $ TH.environments hostnamesByEnvironment
  convertMaybe maybeHostnames "environment missing hostnames"

getConfigHost :: (MonadThrowHttp m) => TH.Hostnames -> m String
getConfigHost selectedHostnames = do
  let maybeConfigUrl = TH.appCastUrl selectedHostnames
  convertMaybe maybeConfigUrl "config url is missing from hostnames"

getPeFile :: (MonadWriteThrowHttp m) => String -> String -> String -> m I.ByteString
getPeFile configHost platform env = do
  let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" configHost platform env
  TF.request peUrl

convertMaybe :: (MonadThrowHttp m) => Maybe a -> String -> m a
convertMaybe val msg = case val of
  Nothing  -> throw $ KeyNotFoundError msg
  (Just x) -> return x

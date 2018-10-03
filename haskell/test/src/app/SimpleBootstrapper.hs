module App.SimpleBootstrapper
  ( bootstrap
  ) where

import           Control.Exception             (throw)
import qualified Data.ByteString.Lazy.Internal as I
import qualified Data.HashMap.Strict           as HMS (lookup)
import           Text.Printf                   (printf)
import           Types.Config                  (Config (Config), environment,
                                                platform, rootUrl)
import qualified Types.Environments            as T
import           Types.Exceptions              (CustomException (KeyNotFoundError))
import qualified Types.Hostnames               as TH
import qualified Utils.SimpleFetch             as SF (jsonRequest, request)

bootstrap :: Config -> IO I.ByteString
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

getEnvironments :: String -> IO T.Environments
getEnvironments rootUrl = SF.jsonRequest $ printf "GET %s" rootUrl

-- TODO: Remove boilderplate key lookup stuff
selectEnvironment :: T.Environments -> String -> IO T.Environment
selectEnvironment environments env = do
  let maybeEnvironment = HMS.lookup env $ T.environments environments
  case maybeEnvironment of
    Nothing  -> throw $ KeyNotFoundError "target environment doesn't exist"
    (Just x) -> return x

getHostnames :: String -> String -> IO TH.HostnameEnvironments
getHostnames configHostname platform = do
  let hostnamesUrl = printf "GET %s/env-list/%s-sling.json" configHostname platform
  SF.jsonRequest hostnamesUrl

selectHostnames :: TH.HostnameEnvironments -> String -> IO TH.Hostnames
selectHostnames hostnamesByEnvironment env = do
  let maybeHostnames = HMS.lookup env $ TH.environments hostnamesByEnvironment
  case maybeHostnames of
    Nothing  -> throw $ KeyNotFoundError "environment missing hostnames"
    (Just x) -> return x

getConfigHost :: TH.Hostnames -> IO String
getConfigHost selectedHostnames =
  case TH.appCastUrl selectedHostnames of
    Nothing  -> throw $ KeyNotFoundError "config url is missing from hostnames"
    (Just x) -> return x

getPeFile :: String -> String -> String -> IO I.ByteString
getPeFile configHost platform env = do
  let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" configHost platform env
  SF.request peUrl

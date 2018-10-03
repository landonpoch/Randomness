module App.TracedBootstrapper
  ( bootstrap
  )
  where

import           Control.Exception             (Exception, throw)
import           Control.Monad.Writer          (lift)
import qualified Data.ByteString.Lazy.Internal as I
import qualified Data.HashMap.Strict           as HMS (lookup)
import           Data.Typeable                 (Typeable)
import           Text.Printf                   (printf)
import           Types.Config                  (Config (Config), environment,
                                                platform, rootUrl)
import qualified Types.Environments            as T
import qualified Types.Hostnames               as TH
import qualified Utils.TracedFetch             as TF (WIO, jsonRequest, request)

data MyException = ThisException String | ThatException
  deriving (Show, Typeable)

instance Exception MyException

bootstrap :: Config -> TF.WIO I.ByteString
bootstrap config = do
  let targetEnvironment = environment config
  let targetPlatform = platform config
  environments <- getEnvironments $ rootUrl config
  selectedEnv <- lift $ selectEnvironment environments targetEnvironment
  hostnamesByEnvironment <- getHostnames (T.configHost selectedEnv) targetPlatform
  selectedHostnames <- lift $ selectHostnames hostnamesByEnvironment targetEnvironment
  configHostname <- lift $ getConfigHost selectedHostnames
  -- TODO: If this exception occurs, nothing is added to the writer at all
  -- throw $ ThisException "Error happened!"
  getPeFile configHostname targetPlatform targetEnvironment
  -- TODO: Parse PE file

getEnvironments :: String -> TF.WIO T.Environments
getEnvironments rootUrl = TF.jsonRequest $ printf "GET %s" rootUrl

selectEnvironment :: T.Environments -> String -> IO T.Environment
selectEnvironment environments env = do
  let maybeEnvironment = HMS.lookup env $ T.environments environments
  case maybeEnvironment of
    Nothing  -> fail "target environment doesn't exist"
    (Just x) -> return x

getHostnames :: String -> String -> TF.WIO TH.HostnameEnvironments
getHostnames configHostname platform = do
  let hostnamesUrl = printf "GET %s/env-list/%s-sling.json" configHostname platform
  TF.jsonRequest hostnamesUrl

selectHostnames :: TH.HostnameEnvironments -> String -> IO TH.Hostnames
selectHostnames hostnamesByEnvironment env = do
  let maybeHostnames = HMS.lookup env $ TH.environments hostnamesByEnvironment
  case maybeHostnames of
    Nothing  -> fail "environment missing hostnames"
    (Just x) -> return x

getConfigHost :: TH.Hostnames -> IO String
getConfigHost selectedHostnames =
  case TH.appCastUrl selectedHostnames of
    Nothing  -> fail "config url is missing from hostnames"
    (Just x) -> return x

getPeFile :: String -> String -> String -> TF.WIO I.ByteString
getPeFile configHost platform env = do
  let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" configHost platform env
  TF.request peUrl

module App.TracedErrBootstrapper
  ( bootstrap
  ) where

import           Control.Monad.Except          (throwError)
import qualified Data.ByteString.Lazy.Internal as I
import qualified Data.HashMap.Strict           as HMS (lookup)
import qualified Data.Text.Lazy.Encoding       as T
import qualified Data.Text.Lazy.IO             as T
import           Text.Printf                   (printf)
import           Types.Config                  (Config, environment, platform,
                                                rootUrl)
import qualified Types.Environments            as T
import           Types.Exceptions              (Error (JsonParseError, KeyNotFoundError))
import qualified Types.Hostnames               as TH
import           Utils.TracedErrFetch          (EWIO, jsonRequest, request)

-- TODO: Take a look at IO Exceptions instead of ExceptT as an alternative
-- see: https://softwareengineering.stackexchange.com/questions/252977/cleanest-way-to-report-errors-in-haskell
-- see: https://www.schoolofhaskell.com/user/commercial/content/exceptions-best-practices
bootstrap :: Config -> EWIO I.ByteString
bootstrap config = do
  let targetEnvironment = environment config
  let targetPlatform = platform config
  environments <- getEnvironments $ rootUrl config
  selectedEnv <- selectEnvironment environments targetEnvironment
  hostnamesByEnvironment <- getHostnames (T.configHost selectedEnv) targetPlatform
  selectedHostnames <- selectHostnames hostnamesByEnvironment targetEnvironment
  configHostname <- getConfigHost selectedHostnames
  peXml <- getPeFile configHostname targetPlatform targetEnvironment
  return $ parsePeFile peXml

getEnvironments :: String -> EWIO T.Environments
getEnvironments rootUrl = jsonRequest $ printf "GET %s" rootUrl

selectEnvironment :: T.Environments -> String -> EWIO T.Environment
selectEnvironment environments env = do
  let maybeEnvironment = HMS.lookup env $ T.environments environments
  toExceptT maybeEnvironment $ KeyNotFoundError "target environment doesn't exist"

getHostnames :: String -> String -> EWIO TH.HostnameEnvironments
getHostnames configHostname platform = do
  let hostnamesUrl = printf "GET %s/env-list/%s-sling.json" configHostname platform
  jsonRequest hostnamesUrl

selectHostnames :: TH.HostnameEnvironments -> String -> EWIO TH.Hostnames
selectHostnames hostnamesByEnvironment env = do
  let maybeHostnames = HMS.lookup env $ TH.environments hostnamesByEnvironment
  toExceptT maybeHostnames $ KeyNotFoundError "environment missing hostnames"

getConfigHost :: TH.Hostnames -> EWIO String
getConfigHost selectedHostnames = toExceptT (TH.appCastUrl selectedHostnames) $ KeyNotFoundError "config url is missing from hostnames"

getPeFile :: String -> String -> String -> EWIO I.ByteString
getPeFile configHost platform env = do
  let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" configHost platform env
  request peUrl

parsePeFile :: I.ByteString -> I.ByteString
parsePeFile = id -- TODO: parse the xml file and return a new ADT

toExceptT :: Maybe a -> Error -> EWIO a
toExceptT m err = case m of
  Nothing  -> throwError err
  (Just x) -> return x

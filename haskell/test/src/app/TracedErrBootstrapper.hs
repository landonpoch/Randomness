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
import           Types.Exceptions              (CustomException (..))
import           Types.Global                  (MonadErrorWriteThrowHttp)
import qualified Types.Hostnames               as TH
import           Utils.TracedErrFetch          (jsonRequest, request)

-- TODO: Maybe these should also use typeclasses and the transformers should
-- only be referenced in the initial call in from main?
-- TODO: Take a look at IO Exceptions instead of ExceptT as an alternative
-- see: https://softwareengineering.stackexchange.com/questions/252977/cleanest-way-to-report-errors-in-haskell
-- see: https://www.schoolofhaskell.com/user/commercial/content/exceptions-best-practices
bootstrap :: (MonadErrorWriteThrowHttp m) => Config -> m I.ByteString
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

getEnvironments :: (MonadErrorWriteThrowHttp m) => String -> m T.Environments
getEnvironments rootUrl = jsonRequest $ printf "GET %s" rootUrl

selectEnvironment :: (MonadErrorWriteThrowHttp m) => T.Environments -> String -> m T.Environment
selectEnvironment environments env = do
  let maybeEnvironment = HMS.lookup env $ T.environments environments
  toExceptT maybeEnvironment "target environment doesn't exist"

getHostnames :: (MonadErrorWriteThrowHttp m) => String -> String -> m TH.HostnameEnvironments
getHostnames configHostname platform = do
  let hostnamesUrl = printf "GET %s/env-list/%s-sling.json" configHostname platform
  jsonRequest hostnamesUrl

selectHostnames :: (MonadErrorWriteThrowHttp m) => TH.HostnameEnvironments -> String -> m TH.Hostnames
selectHostnames hostnamesByEnvironment env = do
  let maybeHostnames = HMS.lookup env $ TH.environments hostnamesByEnvironment
  toExceptT maybeHostnames "environment missing hostnames"

getConfigHost :: (MonadErrorWriteThrowHttp m) => TH.Hostnames -> m String
getConfigHost selectedHostnames = do
  let maybeHostnames = TH.appCastUrl selectedHostnames
  toExceptT maybeHostnames "config url is missing from hostnames"

getPeFile :: (MonadErrorWriteThrowHttp m) => String -> String -> String -> m I.ByteString
getPeFile configHost platform env = do
  let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" configHost platform env
  request peUrl

parsePeFile :: I.ByteString -> I.ByteString
parsePeFile = id -- TODO: parse the xml file and return a new ADT

toExceptT :: (MonadErrorWriteThrowHttp m) => Maybe a -> String -> m a
toExceptT m msg = case m of
  Nothing  -> throwError $ KeyNotFoundError msg
  (Just x) -> return x
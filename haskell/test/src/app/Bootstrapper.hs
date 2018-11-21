{-# LANGUAGE OverloadedStrings #-}

module App.Bootstrapper
  ( bootstrap
  ) where
import           App.Decryption          (decryptPeFile, getPeKey)
import           Control.Exception       (throw)
import           Control.Monad.Catch     (MonadThrow)
import           Crypto.Cipher.AES       (AES128)
import           Crypto.Cipher.Types     (BlockCipher (..), Cipher (..), IV,
                                          makeIV)
import qualified Crypto.Random.Types     as CRT
import           Data.ByteArray          (ByteArray)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy.Encoding as TLE
import           Text.Printf             (printf)
import           Types.Config            (Config (..), environment, platform,
                                          rootUrl)
import qualified Types.Environments      as T
import           Types.Exceptions        (CustomException (..))
import           Types.Global            (MonadFile, MonadHttp, MonadLogger,
                                          trace)
import qualified Types.Hostnames         as TH
import           Utils.Fetch             (jsonRequest, request)

bootstrap :: (MonadFile m, MonadHttp m, MonadThrow m, MonadLogger m)
  => Config -> m T.Text
bootstrap config = do
  let targetEnvironment = environment config
  let targetPlatform = platform config
  environments <- getEnvironments $ rootUrl config
  selectedEnv <- selectEnvironment environments targetEnvironment
  hostnamesByEnvironment <- getHostnames (T.configHost selectedEnv) targetPlatform
  selectedHostnames <- selectHostnames hostnamesByEnvironment targetEnvironment
  -- configHostname <- getConfigHost selectedHostnames
  let configHostname = (T.configHost selectedEnv)
  -- throw RandomException
  peFile <- getPeFile configHostname targetPlatform targetEnvironment
  -- TODO: Parse PE file
  val <- getConsumerKeyAndSecret peFile
  return $ fst val

getEnvironments :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> m T.Environments
getEnvironments rootUrl = jsonRequest . T.pack $ printf "GET %s" rootUrl

getHostnames :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> T.Text -> m TH.HostnameEnvironments
getHostnames configHostname platform = do
  let hostnamesUrl = printf "GET %s/env-list/%s-sling.json" configHostname platform
  jsonRequest $ T.pack hostnamesUrl

getPeFile :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> T.Text -> T.Text -> m T.Text
getPeFile configHost platform env = do
  let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" configHost platform env
  fmap (TE.decodeUtf8 . BS.concat . BL.toChunks) (request $ T.pack peUrl)

getConsumerKeyAndSecret :: (MonadFile m, MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> m (T.Text, T.Text)
getConsumerKeyAndSecret text = do
  key <- getPeKey
  case decryptPeFile key text of
    Left e  -> throw e
    Right t -> trace t
  -- TODO: Parse PE file
  return (text, text)

selectEnvironment :: (MonadThrow m) => T.Environments -> T.Text -> m T.Environment
selectEnvironment environments env = do
  let maybeEnvironment = HM.lookup env $ T.environments environments
  convertMaybe maybeEnvironment "target environment doesn't exist"

selectHostnames :: (MonadThrow m) => TH.HostnameEnvironments -> T.Text -> m TH.Hostnames
selectHostnames hostnamesByEnvironment env = do
  let maybeHostnames = HM.lookup env $ TH.environments hostnamesByEnvironment
  convertMaybe maybeHostnames "environment missing hostnames"

getConfigHost :: (MonadThrow m) => TH.Hostnames -> m T.Text
getConfigHost selectedHostnames = do
  let maybeConfigUrl = TH.appCastUrl selectedHostnames
  convertMaybe maybeConfigUrl "config url is missing from hostnames"

convertMaybe :: (MonadThrow m) => Maybe a -> T.Text -> m a
convertMaybe val msg = case val of
  Nothing  -> throw $ KeyNotFoundError msg
  (Just x) -> return x

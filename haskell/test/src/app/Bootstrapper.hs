{-# LANGUAGE OverloadedStrings #-}

module App.Bootstrapper
  ( bootstrap
  ) where

import           Control.Exception    (throw)
import           Control.Monad.Catch  (MonadThrow)
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Text.Printf          (printf)
import qualified Types.Auth           as Auth
import           Types.Config         (AppConfig (..), Config (..),
                                       UserConfig (..), environment, platform,
                                       rootUrl)
import qualified Types.Environments   as T
import           Types.Exceptions     (CustomException (..))
import           Types.Global         (MonadFile, MonadHttp, MonadLogger,
                                       MonadSign, trace)
import qualified Types.Hostnames      as TH
import           Utils.Fetch          (AuthDetails (..), UserTokens (..),
                                       authGetJson, authPutForm, getJSON,
                                       getText)
import           Utils.Pe             (decryptPeFile, getSecrets)

bootstrap :: (MonadFile m, MonadHttp m, MonadThrow m, MonadLogger m, MonadSign m)
  => Config -> m T.Text
bootstrap config = do
  let targetEnvironment = environment $ appConfig config
  let targetPlatform = platform $ appConfig config
  environments <- getEnvironments . rootUrl $ appConfig config
  selectedEnv <- selectEnvironment environments targetEnvironment
  hostnamesByEnvironment <- getHostnames (T.configHost selectedEnv) targetPlatform
  selectedHostnames <- selectHostnames hostnamesByEnvironment targetEnvironment
  -- configHostname <- getConfigHost selectedHostnames
  let configHostname = T.configHost selectedEnv
  -- throw RandomException
  peFile <- getPeFile configHostname targetPlatform targetEnvironment
  (consumerKey, consumerSecret) <- getConsumerKeyAndSecret (peKey $ appConfig config) peFile
  let umsHostname = TH.umsUrl selectedHostnames
  let auth = AuthDetails
            { consumerKey = consumerKey
            , consumerSecret = consumerSecret
            , userTokens = Nothing
            }
  response <- authenticate (userConfig config) umsHostname auth
  let userTokens = UserTokens
                   { accessToken = Auth.oauthToken response
                   , tokenSecret = Auth.oauthTokenSecret response
                   }
  let userAuth = auth { userTokens = Just userTokens }
  response <- getUserDetails userAuth umsHostname
  trace . T.pack $ show response
  return ""

getEnvironments :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> m T.Environments
getEnvironments = getJSON

getHostnames :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> T.Text -> m TH.HostnameEnvironments
getHostnames configHostname platform = do
  let url = printf "%s/env-list/%s-sling.json" configHostname platform
  getJSON $ T.pack url

getPeFile :: (MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> T.Text -> T.Text -> m T.Text
getPeFile configHost platform env = do
  let url = printf "%s/%s/sling/pe-%s.xml.enc" configHost platform env
  getText $ T.pack url

getConsumerKeyAndSecret :: (MonadFile m, MonadHttp m, MonadThrow m, MonadLogger m)
  => T.Text -> T.Text -> m (T.Text, T.Text)
getConsumerKeyAndSecret key text = do
  let peContents = case decryptPeFile key text of
                    Left e  -> throw e
                    Right t -> t
  case getSecrets peContents of
    Left e  -> throw e
    Right t -> return t

authenticate :: (MonadHttp m, MonadThrow m, MonadLogger m, MonadSign m) => UserConfig -> T.Text -> AuthDetails -> m Auth.AccessTokenResponse
authenticate user umsHost auth = do
  let url = printf "%s/v3/xauth/access_token.json" umsHost
  resp <- authPutForm auth (T.pack url) [("email", TE.encodeUtf8 $ email user)
                                ,("password", TE.encodeUtf8 $ password user)
                                ,("device_guid", TE.encodeUtf8 $ deviceGuid user)]
  case eitherDecode (BL.fromStrict $ TE.encodeUtf8 resp) of
    Left err  -> throw . JsonParseError $ T.pack err
    Right val -> return val

-- TODO: Wire up this call
getUserDetails :: (MonadHttp m, MonadThrow m, MonadLogger m, MonadSign m) => AuthDetails -> T.Text -> m Auth.UserResponse
getUserDetails auth umsHost = do
  let url = printf "%s/v2/user.json" umsHost
  authGetJson auth (T.pack url)

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
  Nothing -> throw $ KeyNotFoundError msg
  Just x  -> return x

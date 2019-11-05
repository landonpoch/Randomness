{-# LANGUAGE OverloadedStrings #-}

module App.Bootstrapper
  ( bootstrap
  )
where

import           Data.Monoid                    ( (<>) )
import           Protolude
import           Control.Exception              ( throw )
import           Data.Aeson                     ( eitherDecode )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Types.Auth                    as Auth
import           Types.Config                   ( AppConfig(..)
                                                , Config(..)
                                                , UserConfig(..)
                                                , environment
                                                , platform
                                                , rootUrl
                                                )
import qualified Types.Environments            as TE
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( MonadFile
                                                , MonadHttp
                                                , MonadLogger
                                                , MonadSign
                                                , debug
                                                )
import qualified Types.Hostnames               as TH
import           Utils.Fetch                    ( AuthDetails(..)
                                                , UserTokens(..)
                                                , authGetJson
                                                , authPutForm
                                                , getJSON
                                                , getText
                                                )
import           Utils.Pe                       ( decryptPeFile
                                                , getSecrets
                                                )

bootstrap
  :: (MonadFile m, MonadHttp m, MonadLogger m, MonadSign m)
  => Config
  -> m T.Text
bootstrap config = do
  let targetEnvironment = environment $ appConfig config
  let targetPlatform    = platform $ appConfig config
  environments <- getEnvironments . rootUrl $ appConfig config
  let selectedEnv = selectEnvironment environments targetEnvironment
  hostnamesByEnvironment <- getHostnames (TE.configHost selectedEnv)
                                         targetPlatform
  let selectedHostnames =
        selectHostnames hostnamesByEnvironment targetEnvironment
  -- configHostname <- getConfigHost selectedHostnames
  let configHostname = TE.configHost selectedEnv
  -- throw RandomException
  peFile <- getPeFile configHostname targetPlatform targetEnvironment
  (consumerKey, consumerSecret) <- getConsumerKeyAndSecret
    (peKey $ appConfig config)
    peFile
  let umsHostname = TH.umsUrl selectedHostnames
  let auth = AuthDetails { consumerKey    = consumerKey
                         , consumerSecret = consumerSecret
                         , userTokens     = Nothing
                         }
  response <- authenticate (userConfig config) umsHostname auth
  let userTokens = UserTokens { accessToken = Auth.oauthToken response
                              , tokenSecret = Auth.oauthTokenSecret response
                              }
  let userAuth = auth { userTokens = Just userTokens }
  response <- getUserDetails userAuth umsHostname
  return $ show response

getEnvironments :: (MonadHttp m, MonadLogger m) => T.Text -> m TE.Environments
getEnvironments = getJSON

getHostnames
  :: (MonadHttp m, MonadLogger m)
  => T.Text
  -> T.Text
  -> m TH.HostnameEnvironments
getHostnames configHostname platform = do
  let url = configHostname <> "/env-list/" <> platform <> "-sling.json"
  getJSON url

getPeFile
  :: (MonadHttp m, MonadLogger m) => T.Text -> T.Text -> T.Text -> m T.Text
getPeFile configHost platform env = do
  let url = configHost <> "/" <> platform <> "/sling/pe-" <> env <> ".xml.enc"
  getText url

getConsumerKeyAndSecret
  :: (MonadFile m, MonadHttp m, MonadLogger m)
  => T.Text
  -> T.Text
  -> m (T.Text, T.Text)
getConsumerKeyAndSecret key text = do
  let peContents = case decryptPeFile key text of
        Left  e -> throw e
        Right t -> t
  debug peContents
  case getSecrets peContents of
    Left  e -> throw e
    Right t -> return t

authenticate
  :: (MonadHttp m, MonadLogger m, MonadSign m)
  => UserConfig
  -> T.Text
  -> AuthDetails
  -> m Auth.AccessTokenResponse
authenticate user umsHost auth = do
  let url = umsHost <> "/v3/xauth/access_token.json"
  resp <- authPutForm
    auth
    url
    [ ("email"      , email user)
    , ("password"   , password user)
    , ("device_guid", deviceGuid user)
    ]
  case eitherDecode (toS resp) of
    Left  err -> throw . JsonParseError $ toS err
    Right val -> return val

getUserDetails
  :: (MonadHttp m, MonadLogger m, MonadSign m)
  => AuthDetails
  -> T.Text
  -> m Auth.UserResponse
getUserDetails auth umsHost = do
  let url = umsHost <> "/v2/user.json"
  authGetJson auth url

selectEnvironment :: TE.Environments -> T.Text -> TE.Environment
selectEnvironment environments env = do
  let maybeEnvironment = HM.lookup env $ TE.environments environments
  convertMaybe maybeEnvironment "target environment doesn't exist"

selectHostnames :: TH.HostnameEnvironments -> T.Text -> TH.Hostnames
selectHostnames hostnamesByEnvironment env = do
  let maybeHostnames = HM.lookup env $ TH.environments hostnamesByEnvironment
  convertMaybe maybeHostnames "environment missing hostnames"

getConfigHost :: TH.Hostnames -> T.Text
getConfigHost selectedHostnames = do
  let maybeConfigUrl = TH.appCastUrl selectedHostnames
  convertMaybe maybeConfigUrl "config url is missing from hostnames"

convertMaybe :: Maybe a -> T.Text -> a
convertMaybe val msg = case val of
  Nothing -> throw $ KeyNotFoundError msg
  Just x  -> x

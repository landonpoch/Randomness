{-# LANGUAGE OverloadedStrings #-}

module App.Bootstrapper
  ( bootstrap
  , bootstrap'
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
import           Types.Global                   ( App(..)
                                                , AppM
                                                , Services(..)
                                                , Services'(..)
                                                , Log(..)
                                                , Log'(..)
                                                , Env(..)
                                                , Env'(..)
                                                )
import qualified Types.Hostnames               as TH
import           Utils.Fetch                    ( AuthDetails(..)
                                                , UserTokens(..)
                                                , authGetJson
                                                , authGetJson'
                                                , authPutForm
                                                , authPutForm'
                                                , getJSON
                                                , getJSON'
                                                , getText
                                                , getText'
                                                )
import           Utils.Pe                       ( decryptPeFile
                                                , getSecrets
                                                )

bootstrap :: Config -> App Text
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

bootstrap' :: Config -> AppM Text
bootstrap' config = do
  let targetEnvironment = environment $ appConfig config
  let targetPlatform    = platform $ appConfig config
  environments <- getEnvironments' . rootUrl $ appConfig config
  let selectedEnv = selectEnvironment environments targetEnvironment
  hostnamesByEnvironment <- getHostnames' (TE.configHost selectedEnv)
                                          targetPlatform
  let selectedHostnames =
        selectHostnames hostnamesByEnvironment targetEnvironment
  -- configHostname <- getConfigHost selectedHostnames
  let configHostname = TE.configHost selectedEnv
  -- throw RandomException
  peFile <- getPeFile' configHostname targetPlatform targetEnvironment
  (consumerKey, consumerSecret) <- getConsumerKeyAndSecret'
    (peKey $ appConfig config)
    peFile
  let umsHostname = TH.umsUrl selectedHostnames
  let auth = AuthDetails { consumerKey    = consumerKey
                         , consumerSecret = consumerSecret
                         , userTokens     = Nothing
                         }
  response <- authenticate' (userConfig config) umsHostname auth
  let userTokens = UserTokens { accessToken = Auth.oauthToken response
                              , tokenSecret = Auth.oauthTokenSecret response
                              }
  let userAuth = auth { userTokens = Just userTokens }
  response <- getUserDetails' userAuth umsHostname
  return $ show response

getEnvironments :: Text -> App TE.Environments
getEnvironments = getJSON

getEnvironments' :: Text -> AppM TE.Environments
getEnvironments' = getJSON'

getHostnames :: Text -> Text -> App TH.HostnameEnvironments
getHostnames configHostname platform = do
  let url = configHostname <> "/env-list/" <> platform <> "-sling.json"
  getJSON url

getHostnames' :: Text -> Text -> AppM TH.HostnameEnvironments
getHostnames' configHostname platform = do
  let url = configHostname <> "/env-list/" <> platform <> "-sling.json"
  getJSON' url

getPeFile :: Text -> Text -> Text -> App Text
getPeFile configHost platform env = do
  let url = configHost <> "/" <> platform <> "/sling/pe-" <> env <> ".xml.enc"
  getText url

getPeFile' :: Text -> Text -> Text -> AppM Text
getPeFile' configHost platform env = do
  let url = configHost <> "/" <> platform <> "/sling/pe-" <> env <> ".xml.enc"
  getText' url

getConsumerKeyAndSecret :: Text -> Text -> App (Text, Text)
getConsumerKeyAndSecret key text = do
  debug <- fmap (lDebug . logging . services) ask
  let peContents = case decryptPeFile key text of
        Left  e -> throw e
        Right t -> t
  liftIO $ debug peContents
  case getSecrets peContents of
    Left  e -> throw e
    Right t -> return t

getConsumerKeyAndSecret' :: Text -> Text -> AppM (Text, Text)
getConsumerKeyAndSecret' key text = do
  debug <- fmap (lDebug' . logging' . services') ask
  let peContents = case decryptPeFile key text of
        Left  e -> throw e
        Right t -> t
  debug peContents
  case getSecrets peContents of
    Left  e -> throw e
    Right t -> return t

authenticate
  :: UserConfig -> Text -> AuthDetails -> App Auth.AccessTokenResponse
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

authenticate'
  :: UserConfig -> Text -> AuthDetails -> AppM Auth.AccessTokenResponse
authenticate' user umsHost auth = do
  let url = umsHost <> "/v3/xauth/access_token.json"
  resp <- authPutForm'
    auth
    url
    [ ("email"      , email user)
    , ("password"   , password user)
    , ("device_guid", deviceGuid user)
    ]
  case eitherDecode (toS resp) of
    Left  err -> throw . JsonParseError $ toS err
    Right val -> return val

getUserDetails :: AuthDetails -> Text -> App Auth.UserResponse
getUserDetails auth umsHost = do
  let url = umsHost <> "/v2/user.json"
  authGetJson auth url

getUserDetails' :: AuthDetails -> Text -> AppM Auth.UserResponse
getUserDetails' auth umsHost = do
  let url = umsHost <> "/v2/user.json"
  authGetJson' auth url

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types.Hostnames
  ( Hostnames(..)
  , HostnameEnvironments(..)
  )
where

import Protolude ( ($), Monad(return), Show, Maybe )
import Data.Aeson
    ( (.:),
      (.:?),
      withObject,
      object,
      FromJSON(parseJSON),
      KeyValue((.=)),
      ToJSON(toJSON) )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.Text                     as T

newtype HostnameEnvironments = HostnameEnvironments { environments :: HashMap T.Text Hostnames } deriving Show

instance FromJSON HostnameEnvironments where
  parseJSON = withObject "hostnameEnvironments" $ \o -> do
    environments <- o .: "environments"
    return HostnameEnvironments { .. }

instance ToJSON HostnameEnvironments where
  toJSON HostnameEnvironments {..} = object ["environments" .= environments]

instance ToJSON Hostnames where
  toJSON Hostnames {..} = object ["serviceId" .= serviceId] -- TODO: Finish mapping if necessary

data Hostnames = Hostnames { serviceId         :: !T.Text
                           , chromecastAppId   :: Maybe T.Text
                           , cmsUrl            :: Maybe T.Text
                           , dmsUrl            :: !T.Text
                           , geoUrl            :: !T.Text
                           , timeUrl           :: !T.Text
                           , logUrl            :: Maybe T.Text
                           , cmwUrl            :: Maybe T.Text
                           , umsUrl            :: !T.Text
                           , microUmsUrl       :: !T.Text
                           , cmwNgUrl          :: Maybe T.Text
                           , extAuthUrl        :: !T.Text
                           , websiteUrl        :: Maybe T.Text
                           , appCastUrl        :: Maybe T.Text
                           , launchUrl         :: Maybe T.Text
                           , upgradeUrl        :: Maybe T.Text
                           , statsUrl          :: !T.Text
                           , channelsUrl       :: !T.Text
                           , signUpLayoutUrl   :: !T.Text
                           , adobeHeartbeatUrl :: !T.Text
                           } deriving Show

instance FromJSON Hostnames where
  parseJSON = withObject "hostnames" $ \o -> do
    serviceId         <- o .: "service_id"
    chromecastAppId   <- o .:? "chromecast_app_id"
    cmsUrl            <- o .:? "cms_url"
    dmsUrl            <- o .: "dms_url"
    geoUrl            <- o .: "geo_url"
    timeUrl           <- o .: "time_url"
    logUrl            <- o .:? "log_url"
    cmwUrl            <- o .:? "cmw_url"
    umsUrl            <- o .: "ums_url"
    microUmsUrl       <- o .: "micro_ums_url"
    cmwNgUrl          <- o .:? "cmwng_url"
    extAuthUrl        <- o .: "extauth_url"
    websiteUrl        <- o .:? "website_url"
    appCastUrl        <- o .:? "appcast_url"
    launchUrl         <- o .:? "launch_url"
    upgradeUrl        <- o .:? "upgrade_url"
    statsUrl          <- o .: "stats_url"
    channelsUrl       <- o .: "channels_url"
    signUpLayoutUrl   <- o .: "sign_up_layout_url"
    adobeHeartbeatUrl <- o .: "adobe_heartbeat_url"
    return Hostnames { .. }

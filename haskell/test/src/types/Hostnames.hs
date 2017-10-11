{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Hostnames
    ( Hostnames(..)
    , HostnameEnvironments
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Aeson          (FromJSON, parseJSON, withObject, (.:), (.:?))

data HostnameEnvironments = HostnameEnvironments { environments :: HashMap String Hostnames } deriving Show

instance FromJSON HostnameEnvironments where
    parseJSON = withObject "hostnameEnvironments" $ \o -> do
        environments <- o .: "environments"
        return HostnameEnvironments{..}

data Hostnames = Hostnames { serviceId         :: !String
                           , chromecastAppId   :: !String 
                           , cmsUrl            :: !String
                           , dmsUrl            :: !String
                           , geoUrl            :: !String
                           , timeUrl           :: !String
                           , logUrl            :: !String
                           , cmwUrl            :: !String
                           , umsUrl            :: !String
                           , microUmsUrl       :: !String
                           , cmwNgUrl          :: Maybe String
                           , extAuthUrl        :: !String
                           , websiteUrl        :: !String
                           , appCastUrl        :: !String
                           , launchUrl         :: !String
                           , upgradeUrl        :: !String
                           , statsUrl          :: !String
                           , channelsUrl       :: !String
                           , signUpLayoutUrl   :: !String
                           , adobeHeartbeatUrl :: !String
                           } deriving Show

instance FromJSON Hostnames where
    parseJSON = withObject "hostnames" $ \o -> do
        serviceId         <- o .:  "service_id"
        chromecastAppId   <- o .:  "chromecast_app_id"
        cmsUrl            <- o .:  "cms_url"
        dmsUrl            <- o .:  "dms_url"
        geoUrl            <- o .:  "geo_url"
        timeUrl           <- o .:  "time_url"
        logUrl            <- o .:  "log_url"
        cmwUrl            <- o .:  "cmw_url"
        umsUrl            <- o .:  "ums_url"
        microUmsUrl       <- o .:  "micro_ums_url"
        cmwNgUrl          <- o .:? "cmwng_url"
        extAuthUrl        <- o .:  "extauth_url"
        websiteUrl        <- o .:  "website_url"
        appCastUrl        <- o .:  "appcast_url"
        launchUrl         <- o .:  "launch_url"
        upgradeUrl        <- o .:  "upgrade_url"
        statsUrl          <- o .:  "stats_url"
        channelsUrl       <- o .:  "channels_url"
        signUpLayoutUrl   <- o .:  "sign_up_layout_url"
        adobeHeartbeatUrl <- o .:  "adobe_heartbeat_url"
        return Hostnames{..}
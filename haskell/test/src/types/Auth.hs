{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Types.Auth
  ( AccessTokenResponse(..)
  , UserResponse(..)
  )
where

import           Data.Aeson
import qualified Data.Text                     as T

data AccessTokenResponse = AccessTokenResponse
  { oauthToken       :: T.Text
  , oauthTokenSecret :: T.Text
  , deviceGuid       :: T.Text
  , userGuid         :: T.Text
  , email            :: T.Text
  } deriving Show

instance FromJSON AccessTokenResponse where
  parseJSON = withObject "accessTokenResponse" $ \o -> do
    oauthToken       <- o .: "oauth_token"
    oauthTokenSecret <- o .: "oauth_token_secret"
    deviceGuid       <- o .: "device_guid"
    userGuid         <- o .: "user_guid"
    email            <- o .: "email"
    return AccessTokenResponse { .. }

-- TODO: Add all the fields
data UserResponse = UserResponse
  { email :: T.Text
  } deriving Show

instance FromJSON UserResponse where
  parseJSON = withObject "userResponse" $ \o -> do
    email <- o .: "email"
    return UserResponse { .. }

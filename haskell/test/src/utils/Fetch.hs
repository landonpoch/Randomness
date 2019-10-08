{-# LANGUAGE OverloadedStrings #-}

module Utils.Fetch
  ( getJSON
  , putJSON
  , postJSON
  , getText
  , postForm
  , putForm
  , authPutForm
  , authGetJson
  , AuthDetails(..)
  , UserTokens(..)
  )
where

import           Control.Monad                  ( join )
import           Control.Arrow                  ( (***) )
import           Protolude
import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                )
import           Control.Exception              ( throw )
import           Control.Monad.Catch            ( MonadThrow )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , eitherDecode
                                                , Value
                                                )
import qualified Data.Text                     as T
import           Network.HTTP.Client            ( responseStatus
                                                , responseHeaders
                                                )
import           Network.HTTP.Simple            ( Request(..)
                                                , Response(..)
                                                , getResponseBody
                                                , getResponseHeader
                                                , getResponseHeaders
                                                , parseRequest
                                                , setRequestBodyJSON
                                                , setRequestBodyLBS
                                                , setRequestBodyURLEncoded
                                                , setRequestMethod
                                                )
import           Network.HTTP.Types.Status      ( statusCode )
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( MonadHttp
                                                , MonadLogger
                                                , MonadSign
                                                , Url
                                                , makeRequest
                                                , sign
                                                , debug
                                                )
import           Web.Authenticate.OAuth         ( Credential
                                                , OAuth
                                                , emptyCredential
                                                , newCredential
                                                , newOAuth
                                                , oauthConsumerKey
                                                , oauthConsumerSecret
                                                , signOAuth
                                                )
import           Text.Show.Pretty               ( ppShow )
import           Data.Aeson.Encode.Pretty       ( encodePretty )

-- TODO: Consider being able to enforce userTokens at the API level vs. always optional
data AuthDetails = AuthDetails
  { consumerKey    :: T.Text
  , consumerSecret :: T.Text
  , userTokens     :: Maybe UserTokens
  }

data UserTokens = UserTokens
  { accessToken :: T.Text
  , tokenSecret :: T.Text
  }

authGetJson
  :: (MonadHttp m, MonadThrow m, MonadLogger m, MonadSign m, FromJSON a)
  => AuthDetails
  -> Url
  -> m a
authGetJson auth url =
  setRequestMethod "GET"
    <$> parseRequest (toS url)
    >>= signRequest auth
    >>= jsonRequest

authPutForm
  :: (MonadHttp m, MonadThrow m, MonadLogger m, MonadSign m)
  => AuthDetails
  -> Url
  -> [(T.Text, T.Text)]
  -> m T.Text
authPutForm auth url body =
  -- setRequestBodyURLEncoded automatically sets request method to "POST", so we need to set it back to put after
  setRequestMethod "PUT"
    .   setRequestBodyURLEncoded (fmap (join (***) toS) body) -- maps over list and maps over tuple
    <$> parseRequest (toS url)
    >>= signRequest auth
    >>= genericRequest

signRequest :: (MonadSign m) => AuthDetails -> Request -> m Request
signRequest auth req = do
  let oauth = newOAuth { oauthConsumerKey    = toS $ consumerKey auth
                       , oauthConsumerSecret = toS $ consumerSecret auth
                       }
  let creds = case userTokens auth of
        Just tokens ->
          newCredential (toS $ accessToken tokens) (toS $ tokenSecret tokens)
        Nothing -> emptyCredential
  sign oauth creds req

getText :: (MonadHttp m, MonadThrow m, MonadLogger m) => Url -> m T.Text
getText url = initRequest url "GET" >>= genericRequest

postForm
  :: (MonadHttp m, MonadThrow m, MonadLogger m) => Url -> T.Text -> m T.Text
postForm url body =
  setRequestBodyLBS (toSL body) <$> initRequest url "POST" >>= genericRequest

putForm
  :: (MonadHttp m, MonadThrow m, MonadLogger m) => Url -> T.Text -> m T.Text
putForm url body =
  setRequestBodyLBS (toSL body) <$> initRequest url "PUT" >>= genericRequest

getJSON :: (MonadHttp m, MonadThrow m, MonadLogger m, FromJSON a) => Url -> m a
getJSON url = initRequest url "GET" >>= jsonRequest

postJSON
  :: (MonadHttp m, MonadThrow m, MonadLogger m, ToJSON a, FromJSON b)
  => Url
  -> a
  -> m b
postJSON url body =
  setRequestBodyJSON body <$> initRequest url "POST" >>= jsonRequest

putJSON
  :: (MonadHttp m, MonadThrow m, MonadLogger m, ToJSON a, FromJSON b)
  => Url
  -> a
  -> m b
putJSON url body =
  setRequestBodyJSON body <$> initRequest url "PUT" >>= jsonRequest

jsonRequest
  :: (MonadHttp m, MonadThrow m, MonadLogger m, FromJSON a) => Request -> m a
jsonRequest request = do
  resp <- genericRequest request
  case eitherDecode $ toSL resp of
    Left  err -> throw . JsonParseError $ toS err
    Right a   -> return a

initRequest :: (MonadThrow m, MonadLogger m) => Url -> T.Text -> m Request
initRequest url verb =
  setRequestMethod (toS verb) <$> parseRequest (T.unpack url)

genericRequest
  :: (MonadHttp m, MonadThrow m, MonadLogger m) => Request -> m T.Text
genericRequest request = do
  debug $ show request
  response <- makeRequest request
  let status       = statusCode $ responseStatus response
  let responseBody = toS $ getResponseBody response
  debugResponse response responseBody
  if status == 200
    then return responseBody
    else throw $ HttpBadStatusCode status

-- TODO: Consider alternative pretty printing options
-- https://www.reddit.com/r/haskell/comments/8ilw75/there_are_too_many_prettyprinting_libraries/
debugResponse :: (MonadLogger m, MonadThrow m) => Response a -> T.Text -> m ()
debugResponse resp body = do
  debug . toS . ppShow $ getResponseHeaders resp
  let contentType = getResponseHeader "Content-Type" resp
  if "application/json; charset=utf-8" `elem` contentType
    then formatJson body >>= debug
    else debug body

formatJson :: (MonadThrow m) => T.Text -> m T.Text
formatJson body = case (eitherDecode $ toSL body) :: Either [Char] Value of
  Left  err -> throw . JsonParseError $ toS err
  Right a   -> return $ toS (encodePretty a)

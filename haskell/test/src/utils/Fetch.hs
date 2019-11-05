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

import           Protolude
import           Control.Exception              ( throw )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , eitherDecode
                                                , Value
                                                )
import qualified Data.Text                     as T
import qualified Data.List                     as L
import           Network.HTTP.Client            ( responseStatus )
import           Network.HTTP.Simple            ( Request(..)
                                                , Response(..)
                                                , getResponseBody
                                                , getResponseHeader
                                                , getResponseHeaders
                                                , setRequestBodyJSON
                                                , setRequestBodyLBS
                                                )
import           Network.HTTP.Types.Status      ( statusCode )
import           Network.HTTP.Types.Header      ( HeaderName )
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( MonadHttp
                                                , MonadLogger
                                                , MonadSign
                                                , Url
                                                , makeRequest
                                                , sign
                                                , debug
                                                , info
                                                , initRequest
                                                , initAuthPut
                                                )
import           Web.Authenticate.OAuth         ( emptyCredential
                                                , newCredential
                                                , newOAuth
                                                , oauthConsumerKey
                                                , oauthConsumerSecret
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.Text.Prettyprint.Doc      ( line
                                                , vsep
                                                , (<+>)
                                                , indent
                                                , pretty
                                                , Doc
                                                )
import           Data.CaseInsensitive           ( CI(original) )

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
  :: (MonadHttp m, MonadLogger m, MonadSign m, FromJSON a)
  => AuthDetails
  -> Url
  -> m a
authGetJson auth url =
  initRequest url "GET" >>= signRequest auth >>= jsonRequest

authPutForm
  :: (MonadHttp m, MonadLogger m, MonadSign m)
  => AuthDetails
  -> Url
  -> [(T.Text, T.Text)]
  -> m T.Text
authPutForm auth url body =
  initAuthPut url body >>= signRequest auth >>= genericRequest

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

getText :: (MonadHttp m, MonadLogger m) => Url -> m T.Text
getText url = initRequest url "GET" >>= genericRequest

postForm :: (MonadHttp m, MonadLogger m) => Url -> T.Text -> m T.Text
postForm url body =
  setRequestBodyLBS (toSL body) <$> initRequest url "POST" >>= genericRequest

putForm :: (MonadHttp m, MonadLogger m) => Url -> T.Text -> m T.Text
putForm url body =
  setRequestBodyLBS (toSL body) <$> initRequest url "PUT" >>= genericRequest

getJSON :: (MonadHttp m, MonadLogger m, FromJSON a) => Url -> m a
getJSON url = initRequest url "GET" >>= jsonRequest

postJSON
  :: (MonadHttp m, MonadLogger m, ToJSON a, FromJSON b) => Url -> a -> m b
postJSON url body =
  setRequestBodyJSON body <$> initRequest url "POST" >>= jsonRequest

putJSON :: (MonadHttp m, MonadLogger m, ToJSON a, FromJSON b) => Url -> a -> m b
putJSON url body =
  setRequestBodyJSON body <$> initRequest url "PUT" >>= jsonRequest

jsonRequest :: (MonadHttp m, MonadLogger m, FromJSON a) => Request -> m a
jsonRequest request = do
  resp <- genericRequest request
  case eitherDecode $ toSL resp of
    Left  err -> throw . JsonParseError $ toS err
    Right a   -> return a

genericRequest :: (MonadHttp m, MonadLogger m) => Request -> m T.Text
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
debugResponse :: (MonadLogger m) => Response a -> T.Text -> m ()
debugResponse resp body = do
  let status = statusCode $ responseStatus resp
  info . show $ "Response Status:" <+> pretty status <> line
  debug . formatHeaders $ getResponseHeaders resp
  if isJsonResponse resp
    then debug $ formatJson body
    else debug . show $ "Response Body:" <> line <> pretty body <> line

isJsonResponse :: Response a -> Bool
isJsonResponse resp = do
  let contentType = getResponseHeader "Content-Type" resp
  or $ fmap (\x -> L.isInfixOf "application/json" (toS x)) contentType

formatHeaders :: [(HeaderName, ByteString)] -> T.Text
formatHeaders headers = do
  let mappedHeaders = fmap headerToDoc headers
  show $ "Response Headers:" <> line <> indent 4 (vsep mappedHeaders) <> line

headerToDoc :: (HeaderName, ByteString) -> Doc ann
headerToDoc (key, value) =
  (pretty keyText) <> ":" <+> (pretty $ (toS value :: T.Text))
  where keyText = (toS $ original key) :: T.Text

formatJson :: T.Text -> T.Text
formatJson body = case (eitherDecode $ toSL body) :: Either [Char] Value of
  Left  err -> throw . JsonParseError $ toS err
  Right a   -> do
    let rawVal = toS (encodePretty a) :: T.Text
    show $ "Response Body:" <> line <> pretty rawVal <> line

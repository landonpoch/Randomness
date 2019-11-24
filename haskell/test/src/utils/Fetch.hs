{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.Fetch
  ( getJSON
  , getJSON'
  , putJSON
  , postJSON
  , getText
  , getText'
  , postForm
  , putForm
  , authPutForm
  , authPutForm'
  , authGetJson
  , authGetJson'
  , AuthDetails(..)
  , UserTokens(..)
  )
where

import           Protolude
import           Control.Arrow                  ( (***) )
import           Control.Exception              ( throw )
import           Control.Monad.Catch            ( MonadThrow )
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
                                                , setRequestMethod
                                                , parseRequest
                                                , setRequestBodyURLEncoded
                                                )
import           Network.HTTP.Types.Status      ( statusCode )
import           Network.HTTP.Types.Header      ( HeaderName )
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( Url
                                                , App
                                                , AppM
                                                , Env(..)
                                                , Env'(..)
                                                , Services(..)
                                                , Services'(..)
                                                , Log(..)
                                                , Log'(..)
                                                , Http(..)
                                                , Http'(..)
                                                , Signer(..)
                                                , Signer'(..)
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
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                )

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

initRequest :: (MonadIO m, MonadThrow m) => Url -> Text -> m Request
initRequest url verb = setRequestMethod (toS verb) <$> parseRequest (toS url)

initAuthPut :: (MonadIO m, MonadThrow m) => Url -> [(Text, Text)] -> m Request
initAuthPut url body =
  setRequestMethod "PUT"
    .   setRequestBodyURLEncoded (fmap (join (***) toS) body) -- maps over list and maps over tuple
    <$> parseRequest (toS url)

authGetJson :: FromJSON a => AuthDetails -> Url -> App a
authGetJson auth url =
  initRequest url "GET" >>= signRequest auth >>= jsonRequest

authGetJson' :: FromJSON a => AuthDetails -> Url -> AppM a
authGetJson' auth url =
  initRequest url "GET" >>= signRequest' auth >>= jsonRequest'

authPutForm :: AuthDetails -> Url -> [(Text, Text)] -> App Text
authPutForm auth url body =
  initAuthPut url body >>= signRequest auth >>= genericRequest

authPutForm' :: AuthDetails -> Url -> [(Text, Text)] -> AppM Text
authPutForm' auth url body =
  initAuthPut url body >>= signRequest' auth >>= genericRequest'

signRequest :: AuthDetails -> Request -> App Request
signRequest auth req = do
  sign <- fmap (sSign . signer . services) ask
  let oauth = newOAuth { oauthConsumerKey    = toS $ consumerKey auth
                       , oauthConsumerSecret = toS $ consumerSecret auth
                       }
  let creds = case userTokens auth of
        Just tokens ->
          newCredential (toS $ accessToken tokens) (toS $ tokenSecret tokens)
        Nothing -> emptyCredential
  sign oauth creds req

signRequest' :: AuthDetails -> Request -> AppM Request
signRequest' auth req = do
  sign <- fmap (sSign' . signer' . services') ask
  let oauth = newOAuth { oauthConsumerKey    = toS $ consumerKey auth
                       , oauthConsumerSecret = toS $ consumerSecret auth
                       }
  let creds = case userTokens auth of
        Just tokens ->
          newCredential (toS $ accessToken tokens) (toS $ tokenSecret tokens)
        Nothing -> emptyCredential
  sign oauth creds req

getText :: Url -> App Text
getText url = initRequest url "GET" >>= genericRequest

getText' :: Url -> AppM Text
getText' url = initRequest url "GET" >>= genericRequest'

postForm :: Url -> Text -> App Text
postForm url body =
  setRequestBodyLBS (toSL body) <$> initRequest url "POST" >>= genericRequest

postForm' :: Url -> Text -> AppM Text
postForm' url body =
  setRequestBodyLBS (toSL body) <$> initRequest url "POST" >>= genericRequest'

putForm :: Url -> Text -> App Text
putForm url body =
  setRequestBodyLBS (toSL body) <$> initRequest url "PUT" >>= genericRequest

putForm' :: Url -> Text -> AppM Text
putForm' url body =
  setRequestBodyLBS (toSL body) <$> initRequest url "PUT" >>= genericRequest'

getJSON :: FromJSON a => Url -> App a
getJSON url = initRequest url "GET" >>= jsonRequest

getJSON' :: FromJSON a => Url -> AppM a
getJSON' url = initRequest url "GET" >>= jsonRequest'

postJSON :: (ToJSON a, FromJSON b) => Url -> a -> App b
postJSON url body =
  setRequestBodyJSON body <$> initRequest url "POST" >>= jsonRequest

postJSON' :: (ToJSON a, FromJSON b) => Url -> a -> AppM b
postJSON' url body =
  setRequestBodyJSON body <$> initRequest url "POST" >>= jsonRequest'

putJSON :: (ToJSON a, FromJSON b) => Url -> a -> App b
putJSON url body =
  setRequestBodyJSON body <$> initRequest url "PUT" >>= jsonRequest

putJSON' :: (ToJSON a, FromJSON b) => Url -> a -> AppM b
putJSON' url body =
  setRequestBodyJSON body <$> initRequest url "PUT" >>= jsonRequest'

jsonRequest :: FromJSON a => Request -> App a
jsonRequest request = do
  resp <- genericRequest request
  case eitherDecode $ toSL resp of
    Left  err -> throw . JsonParseError $ toS err
    Right a   -> return a

jsonRequest' :: FromJSON a => Request -> AppM a
jsonRequest' request = do
  resp <- genericRequest' request
  case eitherDecode $ toSL resp of
    Left  err -> throw . JsonParseError $ toS err
    Right a   -> return a

genericRequest :: Request -> App Text
genericRequest request = do
  srv <- fmap services ask
  let debug       = lDebug $ logging srv
  let makeRequest = hMakeRequest $ http srv
  -- debug $ show request
  response <- makeRequest request
  let status       = statusCode $ responseStatus response
  let responseBody = toS $ getResponseBody response
  debugResponse response responseBody
  if status == 200
    then return responseBody
    else throw $ HttpBadStatusCode status

genericRequest' :: Request -> AppM Text
genericRequest' request = do
  srv <- fmap services' ask
  let debug       = lDebug' $ logging' srv
  let makeRequest = hMakeRequest' $ http' srv
  debug $ show request
  response <- makeRequest request
  let status       = statusCode $ responseStatus response
  let responseBody = toS $ getResponseBody response
  debugResponse'' response responseBody
  if status == 200
    then return responseBody
    else throw $ HttpBadStatusCode status

-- TODO: Consider alternative pretty printing options
-- https://www.reddit.com/r/haskell/comments/8ilw75/there_are_too_many_prettyprinting_libraries/
debugResponse :: Response a -> Text -> App ()
debugResponse resp body = do
  env <- ask
  let logger  = logging $ services env
  let lInfo'  = lInfo logger
  let lDebug' = lDebug logger
  let status  = statusCode $ responseStatus resp
  -- lInfo' . show $ "Response Status:" <+> pretty status <> line
  -- lDebug' . formatHeaders $ getResponseHeaders resp
  -- if isJsonResponse resp
  --   then lDebug' $ formatJson body
  --   else lDebug' . show $ "Response Body:" <> line <> pretty body <> line
  return ()

debugResponse' :: (MonadReader Env m, MonadIO m) => Response a -> Text -> m ()
debugResponse' resp body = do
  env <- ask
  let logger  = logging $ services env
  let lInfo'  = lInfo logger
  let lDebug' = lDebug logger
  let status  = statusCode $ responseStatus resp
  liftIO $ lInfo' . show $ "Response Status:" <+> pretty status <> line
  liftIO $ lDebug' . formatHeaders $ getResponseHeaders resp
  if isJsonResponse resp
    then liftIO $ lDebug' $ formatJson body
    else
      liftIO $ lDebug' . show $ "Response Body:" <> line <> pretty body <> line

debugResponse'' :: Response a -> Text -> AppM ()
debugResponse'' resp body = do
  logger <- fmap (logging' . services') ask
  let info   = lInfo' logger
  let debug  = lDebug' logger
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

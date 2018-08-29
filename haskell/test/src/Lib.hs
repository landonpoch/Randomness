module Lib
    ( requestJSON
    , printRequest
    , traceRequest
    , tracedRequest
    , plainRequest
    , textRequest
    , Tracer
    , TracedRequest
    , Url
    ) where

import           Control.Arrow              (left)
import           Control.Exception          (displayException)
import           Control.Monad              ((<=<), (>>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT)
import           Control.Monad.Writer
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack, putStrLn)
import           Network.HTTP.Simple        (Request, Response, getResponseBody,
                                             httpJSONEither, httpLBS,
                                             parseRequest)

type Url = String
requestJSON :: (FromJSON a) => Url -> IO (Either String a)
requestJSON url = fmap (left displayException) (request httpJSONEither url)

printRequest :: Url -> IO ()
printRequest url = do
    putStrLn url
    L8.putStrLn <=< request httpLBS $ url

out :: (FromJSON a) => L8.ByteString -> Writer L8.ByteString (Either String a)
out val = do
  tell val
  return $ eitherDecode val

type Tracer a = WriterT L8.ByteString IO (Either String a)
tracedRequest :: (FromJSON a) => Url -> Tracer a
tracedRequest url = do
  tell $ L8.pack $ url ++ "\n"
  resp <- lift (request httpLBS url)
  tell resp
  return $ eitherDecode resp

type TracedRequest a = Writer [L8.ByteString] (Either String a)
traceRequest :: (FromJSON a) => Url -> IO (TracedRequest a)
traceRequest url = do
    resp <- request httpLBS url
    return $  tell [L8.pack url]
           >> tell [resp]
           >> return (eitherDecode resp)

plainRequest :: (FromJSON a) => Url -> EitherT String IO a
plainRequest url = do
  let response = httpLBS <=< parseRequest $ url
  responseBody <- fmap getResponseBody response
  hoistEither $ eitherDecode responseBody

textRequest :: Url -> IO L8.ByteString
textRequest url = do
  let response = httpLBS <=< parseRequest $ url
  responseBody <- fmap getResponseBody response
  return responseBody

request :: (Request -> IO (Response a)) -> Url -> IO a
request requestMechanism url = do
  let response = requestMechanism <=< parseRequest $ url
  fmap getResponseBody response

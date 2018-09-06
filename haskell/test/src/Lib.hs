module Lib
    ( jsonRequest
    , request
    , tracedRequest
    , tracedJsonRequest
    , Url
    , EIO
    , WEIO
    , WIO
    ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Except       (ExceptT, throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Writer       (WriterT, tell)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack)
import           Network.HTTP.Simple        (Request, Response, getResponseBody,
                                             httpLBS, parseRequest)
import           Types.Exceptions           (Error (JsonParseError))

type Url = String
type EIO = ExceptT Error IO
type WEIO = WriterT L8.ByteString EIO
type WIO = WriterT L8.ByteString IO

jsonRequest :: (FromJSON a) => Url -> WEIO a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
       (Left err) -> do
          tell $ L8.pack err
          throwError $ JsonParseError err
       (Right a)  -> return a

request :: Url -> WEIO L8.ByteString
request url = do
  tell (L8.pack (url ++ "\n"))
  response <- fmap getResponseBody (httpLBS <=< parseRequest $ url)
  tell response
  return response

tracedJsonRequest :: (FromJSON a) => Url -> WIO a
tracedJsonRequest url = do
  response <- tracedRequest url
  case eitherDecode response of
    (Left err) -> fail err
    (Right a)  -> return a

tracedRequest :: Url -> WIO L8.ByteString
tracedRequest url = do
  tell (L8.pack (url ++ "\n"))
  response <- fmap getResponseBody (httpLBS <=< parseRequest $url)
  tell response
  return response

module Utils.SimpleFetch
  ( jsonRequest
  , request
  ) where

import           Control.Exception          (Exception, throw)
import           Control.Monad              ((<=<))
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack, unpack)
import           Data.Typeable              (Typeable)
import           Debug.Trace                (traceIO)
import           Network.HTTP.Client        (responseStatus)
import           Network.HTTP.Simple        (getResponseBody, httpLBS,
                                             parseRequest)
import           Network.HTTP.Types.Status  (statusCode)
import           Types.Exceptions           (MyHttpException (HttpBadStatusCode))

type Url = String

jsonRequest :: (FromJSON a) => Url -> IO a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
    (Left err) -> fail err
    (Right a)  -> return a

request :: Url -> IO L8.ByteString
request url = do
  traceIO url
  response <- httpLBS <=< parseRequest $ url
  let status = statusCode $ responseStatus response
  if status == 200 then do
    let responseBody = getResponseBody response
    traceIO $ L8.unpack responseBody
    return responseBody
  else
    throw $ HttpBadStatusCode status

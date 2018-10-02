module Utils.SimpleFetch
  (
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

type Url = String
data MyHttpException = HttpBadStatusCode Int | HttpUnknownException
  deriving (Show, Typeable)
instance Exception MyHttpException

jreq :: (FromJSON a) => Url -> IO a
jreq url = do
  response <- req url
  case eitherDecode response of
    (Left err) -> fail err
    (Right a)  -> return a

req :: Url -> IO L8.ByteString
req url = do
  traceIO url
  throw $ HttpBadStatusCode 500
  response <- httpLBS <=< parseRequest $ url
  let status = statusCode $ responseStatus response
  if status == 200 then do
    let responseBody = getResponseBody response
    traceIO $ L8.unpack responseBody
    return responseBody
  else
    throw $ HttpBadStatusCode status

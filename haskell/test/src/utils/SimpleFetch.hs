module Utils.SimpleFetch
  ( jsonRequest
  , request
  ) where

import           Control.Exception          (throw)
import           Control.Monad              ((<=<))
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, unpack)
import           Debug.Trace                (traceM)
import           Network.HTTP.Client        (responseStatus)
import           Network.HTTP.Simple        (getResponseBody, parseRequest)
import           Network.HTTP.Types.Status  (statusCode)
import           Types.Exceptions           (CustomException (..))
import           Types.Global               (MonadThrowHttp, Url, makeRequest)

-- TODO: Can probably make jsonRequest and request completely generic
-- for all 3 stacks
jsonRequest :: (MonadThrowHttp m, FromJSON a) => Url -> m a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
    (Left err) -> throw $ JsonParseError err
    (Right a)  -> return a

request :: (MonadThrowHttp m) => Url -> m L8.ByteString
request url = do
  traceM url
  response <- makeRequest <=< parseRequest $ url
  let status = statusCode $ responseStatus response
  if status == 200 then do
    let responseBody = getResponseBody response
    traceM $ L8.unpack responseBody
    return responseBody
  else
    throw $ HttpBadStatusCode status

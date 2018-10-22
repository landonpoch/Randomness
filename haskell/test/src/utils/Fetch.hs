module Utils.Fetch
  ( jsonRequest
  , request
  ) where

import           Control.Exception          (throw)
import           Control.Monad              ((<=<))
import           Control.Monad.Catch        (MonadThrow)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, unpack)
import           Network.HTTP.Client        (responseStatus)
import           Network.HTTP.Simple        (getResponseBody, parseRequest)
import           Network.HTTP.Types.Status  (statusCode)
import           Types.Exceptions           (CustomException (..))
import           Types.Global               (MonadHttp, MonadLogger, Url,
                                             makeRequest, trace)

jsonRequest :: (MonadHttp m, MonadThrow m, MonadLogger m, FromJSON a) => Url -> m a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
    (Left err) -> throw $ JsonParseError err
    (Right a)  -> return a

request :: (MonadHttp m, MonadThrow m, MonadLogger m) => Url -> m L8.ByteString
request url = do
  trace url
  response <- makeRequest <=< parseRequest $ url
  let status = statusCode $ responseStatus response
  if status == 200 then do
    let responseBody = getResponseBody response
    trace $ L8.unpack responseBody
    return responseBody
  else
    throw $ HttpBadStatusCode status

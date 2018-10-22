module Utils.Fetch
  ( jsonRequest
  , request
  ) where

import           Control.Exception         (throw)
import           Control.Monad             ((<=<))
import           Control.Monad.Catch       (MonadThrow)
import           Data.Aeson                (FromJSON, eitherDecode)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text.Encoding        as TE
import           Network.HTTP.Client       (responseStatus)
import           Network.HTTP.Simple       (getResponseBody, parseRequest)
import           Network.HTTP.Types.Status (statusCode)
import           Types.Exceptions          (CustomException (..))
import           Types.Global              (MonadHttp, MonadLogger, Url,
                                            makeRequest, trace)

jsonRequest :: (MonadHttp m, MonadThrow m, MonadLogger m, FromJSON a) => Url -> m a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
    (Left err) -> throw $ JsonParseError $ TE.decodeUtf8 $ C.pack err
    (Right a)  -> return a

request :: (MonadHttp m, MonadThrow m, MonadLogger m) => Url -> m BL.ByteString
request url = do
  trace url
  response <- makeRequest <=< parseRequest . C.unpack $ TE.encodeUtf8 url
  let status = statusCode $ responseStatus response
  if status == 200 then do
    let responseBody = getResponseBody response
    let txtResp = TE.decodeUtf8 . BS.concat $ BL.toChunks responseBody
    trace txtResp
    return responseBody
  else
    throw $ HttpBadStatusCode status

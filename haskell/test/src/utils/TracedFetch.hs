module Utils.TracedFetch
  ( jsonRequest
  , request
  ) where

import           Control.Exception          (throw)
import           Control.Monad.Writer       (tell)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack)
import           Types.Exceptions           (CustomException (..))
import           Types.Global               (MonadWriteThrowHttp, Url)
import qualified Utils.SimpleFetch          as SF (request)

jsonRequest :: (MonadWriteThrowHttp m, FromJSON a) => Url -> m a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
    (Left err) -> throw $ JsonParseError err
    (Right a)  -> return a

request :: (MonadWriteThrowHttp m) => Url -> m L8.ByteString
request url = do
  tell $ L8.pack $ url ++ "\n"
  response <- SF.request url
  tell response
  return response

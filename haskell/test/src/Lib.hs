module Lib
    ( jsonRequest
    , request
    , Url
    , EIO
    ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Except       (ExceptT, throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Network.HTTP.Simple        (Request, Response, getResponseBody,
                                             httpLBS, parseRequest)
import           Types.Exceptions           (Error (JsonParseError))

type Url = String
type EIO = ExceptT Error IO

jsonRequest :: (FromJSON a) => Url -> EIO a
jsonRequest url = do
  response <- liftIO $ request url
  case eitherDecode response of
       (Left err) -> throwError $ JsonParseError err
       (Right a)  -> return a

request :: Url -> IO L8.ByteString
request url = do
  let response = httpLBS <=< parseRequest $ url
  fmap getResponseBody response

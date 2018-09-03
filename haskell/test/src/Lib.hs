module Lib
    ( mtlRequest
    , textRequest
    , Url
    ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Except       (ExceptT, throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Network.HTTP.Simple        (Request, Response, getResponseBody,
                                             httpLBS, parseRequest)

type Url = String

textRequest :: Url -> IO L8.ByteString
textRequest url = do
  let response = httpLBS <=< parseRequest $ url
  fmap getResponseBody response

mtlRequest :: (FromJSON a) => Url -> ExceptT String IO a
mtlRequest url = do
  response <- liftIO $ request httpLBS url
  case eitherDecode response of
       (Left err) -> throwError err
       (Right a)  -> return a

request :: (Request -> IO (Response a)) -> Url -> IO a
request requestMechanism url = do
  let response = requestMechanism <=< parseRequest $ url
  fmap getResponseBody response

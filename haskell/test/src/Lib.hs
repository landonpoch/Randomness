module Lib
    ( jsonRequest
    , request
    , Url
    , EIO
    , WEIO
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

jsonRequest :: (FromJSON a) => Url -> WEIO a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
       (Left err) -> throwError $ JsonParseError err
       (Right a)  -> return a

request :: Url -> WEIO L8.ByteString
request url = do
  response <- httpLBS <=< parseRequest $ url
  let parsedResponse = getResponseBody response
  tell (L8.pack (url ++ "\n"))
    >> tell parsedResponse
    >> return parsedResponse

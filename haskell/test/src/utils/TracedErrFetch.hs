module Utils.TracedErrFetch
  ( EWIO
  , request
  , jsonRequest
  ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Except       (ExceptT, lift, throwError)
import           Control.Monad.Writer       (WriterT, tell)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack)
import           Network.HTTP.Simple        (getResponseBody, httpLBS,
                                             parseRequest)
import           Types.Exceptions           (Error (JsonParseError))
import qualified Utils.TracedFetch          as TR (WIO, request)

type Url = String
type EWIO = ExceptT Error TR.WIO

jsonRequest :: (FromJSON a) => Url -> EWIO a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
       (Left err) -> do
          tell $ L8.pack err
          throwError $ JsonParseError err
       (Right a)  -> return a

request :: Url -> EWIO L8.ByteString
request url = lift $ TR.request url

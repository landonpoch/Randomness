module Utils.TracedFetch
  ( WIO
  , jsonRequest
  , request
  ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Writer       (WriterT, lift, tell)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack)
import           Network.HTTP.Simple        (getResponseBody, httpLBS,
                                             parseRequest)
import qualified Utils.SimpleFetch          as SR (request)

type Url = String
type WIO = WriterT L8.ByteString IO

jsonRequest :: (FromJSON a) => Url -> WIO a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
    (Left err) -> fail err
    (Right a)  -> return a

request :: Url -> WIO L8.ByteString
request url = do
  tell (L8.pack (url ++ "\n"))
  response <- lift $ SR.request url
  tell response
  return response

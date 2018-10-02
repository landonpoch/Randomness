module Utils.TracedFetch
  ( WIO
  ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Writer       (WriterT, tell)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack)
import           Network.HTTP.Simple        (getResponseBody, httpLBS,
                                             parseRequest)

type Url = String
type WIO = WriterT L8.ByteString IO

tracedJsonRequest :: (FromJSON a) => Url -> WIO a
tracedJsonRequest url = do
  response <- tracedRequest url
  case eitherDecode response of
    (Left err) -> fail err
    (Right a)  -> return a

tracedRequest :: Url -> WIO L8.ByteString
tracedRequest url = do
  tell (L8.pack (url ++ "\n"))
  response <- fmap getResponseBody (httpLBS <=< parseRequest $ url)
  tell response
  return response

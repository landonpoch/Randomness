module Lib
    ( requestJSON
    , printRequest
    , traceRequest
    , TracedRequest
    ) where

import           Control.Arrow              (left)
import           Control.Exception          (displayException)
import           Control.Monad              ((<=<), (>>))
import           Control.Monad.Writer
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack, putStrLn)
import           Network.HTTP.Simple        (Request, Response, getResponseBody,
                                             httpJSONEither, httpLBS,
                                             parseRequest)

type Url = String
requestJSON :: (FromJSON a) => Url -> IO (Either String a)
requestJSON url = fmap (left displayException) (request httpJSONEither url)

printRequest :: Url -> IO ()
printRequest url = do
    putStrLn url
    L8.putStrLn <=< request httpLBS $ url

type TracedRequest a = Writer [L8.ByteString] (Either String a)
traceRequest :: (FromJSON a) => Url -> IO (TracedRequest a)
traceRequest url = do
    resp <- request httpLBS url
    return $  tell [L8.pack url]
           >> tell [resp]
           >> return (eitherDecode resp)

request :: (Request -> IO (Response a)) -> Url -> IO a
request requestMechanism = fmap getResponseBody . requestMechanism <=< parseRequest

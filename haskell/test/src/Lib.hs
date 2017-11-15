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
import           Network.HTTP.Simple        (JSONException, Request, Response,
                                             getResponseBody, httpJSONEither,
                                             httpLBS, parseRequest)

type Url = String
requestJSON :: (FromJSON a) => Url -> IO (Either String a)
requestJSON url = do
    parsedRequest <- parseRequest url
    test <- httpJSONEither parsedRequest
    let test2 = getResponseBody test
    let test3 = left displayException test2
    return test3

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

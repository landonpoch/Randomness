module Lib
    ( requestJSON
    , printRequest
    ) where

import           Control.Monad                 ( (<=<) )
import           Network.HTTP.Simple           ( getResponseBody, httpJSON, httpLBS, parseRequest, Request, Response )
import           Data.Aeson                    ( FromJSON )
import qualified Data.ByteString.Lazy.Char8 as L8

type Url = String
requestJSON :: (FromJSON a) => Url -> IO (a)
requestJSON = request httpJSON

printRequest :: Url -> IO ()
printRequest = L8.putStrLn <=< request httpLBS

request :: (Request -> IO (Response a)) -> Url -> IO (a)
request requestMechanism = fmap getResponseBody . requestMechanism <=< parseRequest
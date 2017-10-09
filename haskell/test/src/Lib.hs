module Lib
    ( requestJSON
    , printRequest
    ) where

import           Control.Monad                 ( (<=<) )
import           Network.HTTP.Simple           ( getResponseBody, httpJSON, httpLBS, parseRequest )
import           Data.Aeson                    ( FromJSON )
import qualified Data.ByteString.Lazy.Char8 as L8

type Url = String
requestJSON :: (FromJSON a) => Url -> IO (a)
requestJSON = fmap getResponseBody . httpJSON <=< parseRequest

printRequest :: Url -> IO ()
printRequest = L8.putStrLn <=< fmap getResponseBody . httpLBS <=< parseRequest
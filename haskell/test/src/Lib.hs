module Lib
    ( requestJSON
    , printRequest
    ) where

import           Control.Monad
import           Network.HTTP.Simple
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

type Url = String
requestJSON :: (FromJSON a) => Url -> IO (a)
requestJSON = fmap getResponseBody . httpJSON <=< parseRequest

printRequest :: Url -> IO ()
printRequest = L8.putStrLn <=< fmap getResponseBody . httpLBS <=< parseRequest
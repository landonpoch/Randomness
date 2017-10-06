module Lib
    ( bootstrap
    , testUrl
    ) where

import           Control.Monad
import           Network.HTTP.Simple
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

type Url = String
bootstrap :: (FromJSON a) => Url -> IO (a)
bootstrap = fmap getResponseBody . httpJSON <=< parseRequest

testUrl :: Url -> IO ()
testUrl = L8.putStrLn <=< fmap getResponseBody . httpLBS <=< parseRequest
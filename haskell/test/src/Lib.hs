module Lib
    ( bootstrap
    ) where

import           Control.Monad
import           Network.HTTP.Simple
import qualified Types.Environments as T
-- import qualified Data.ByteString.Char8 as S8
-- import qualified Data.Yaml             as Yaml
-- import           Text.Read             (readMaybe)

type Url = String
bootstrap :: Url -> IO (T.Environments)
bootstrap = fmap getResponseBody . httpJSON <=< parseRequest

-- bootstrap :: String -> IO ()
-- bootstrap = S8.putStrLn <=< fmap encodeResponse . httpJSON <=< parseRequest

-- encodeResponse :: Response Value -> S8.ByteString
-- encodeResponse = Yaml.encode . getResponseBody
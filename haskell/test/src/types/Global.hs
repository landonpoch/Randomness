module Types.Global
  ( Url
  , MonadHttp
  , MonadLogger
  , makeRequest
  , trace
  ) where

import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Network.HTTP.Simple        (Request, Response, httpLBS)

type Url = T.Text

class MonadHttp m where
  makeRequest :: Request -> m (Response L8.ByteString)

-- TODO: examine https://github.com/kazu-yamamoto/logger
class MonadLogger m where
  trace :: T.Text -> m ()
  -- TODO: Support for more log levels?

instance MonadHttp IO where
  makeRequest = httpLBS
instance MonadLogger IO where
  trace = TIO.putStrLn

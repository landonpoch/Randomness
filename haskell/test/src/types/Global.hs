module Types.Global
  ( Url
  , MonadHttp
  , MonadLogger
  , makeRequest
  , trace
  ) where

import qualified Control.Monad.Except       as E (ExceptT, liftIO)
import           Control.Monad.Writer       as W (WriterT, liftIO)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Network.HTTP.Simple        (Request, Response, httpLBS)
import           Types.Exceptions           (CustomException (..))

type Url = String -- TODO: Switch to T.Text

class MonadHttp m where
  makeRequest :: Request -> m (Response L8.ByteString)
class MonadLogger m where -- TODO: examine https://github.com/kazu-yamamoto/logger
  trace :: String -> m () -- TODO: Switch to T.Text
  -- TODO: Support for more log levels?

instance MonadHttp IO where
  makeRequest = httpLBS
instance MonadLogger IO where
  trace = putStrLn

module Types.Global
  ( Url
  , MonadHttp
  , MonadLogger
  , MonadFile
  , MonadSign
  , makeRequest
  , debug
  , sign
  , readFile'
  )
where

import           Protolude
import qualified Data.ByteString.Lazy.Char8    as L8
                                                ( ByteString )
import qualified Data.Text                     as T
import qualified Data.Text.IO                   ( readFile )
import           Network.HTTP.Simple            ( Request
                                                , Response
                                                , httpLBS
                                                )
import           Web.Authenticate.OAuth         ( Credential(..)
                                                , OAuth(..)
                                                , emptyCredential
                                                , newOAuth
                                                , oauthConsumerKey
                                                , signOAuth
                                                )

type Url = T.Text

class (Monad m) => MonadHttp m where
  makeRequest :: Request -> m (Response L8.ByteString)

-- TODO: examine https://github.com/kazu-yamamoto/logger
class (Monad m) => MonadLogger m where
  debug :: T.Text -> m ()
  -- TODO: Support for more log levels?

class (Monad m) => MonadFile m where
  readFile' :: T.Text -> m T.Text

class (Monad m) => MonadSign m where
  sign :: OAuth -> Credential -> Request -> m Request

instance MonadHttp IO where
  makeRequest = httpLBS
instance MonadLogger IO where
  debug = putText
instance MonadFile IO where
  readFile' path = readFile (toS path)
instance MonadSign IO where
  sign = signOAuth

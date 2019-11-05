{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances     #-}

module Types.Global
  ( Url
  , MonadHttp
  , MonadLogger
  , MonadFile
  , MonadSign
  , LogLevel(..)
  , makeRequest
  , debug
  -- TODO: See if there's a better way to export these as a cohesive unit
  , info
  , warn
  , error
  , fatal
  , sign
  , readFile'
  , initRequest
  , initAuthPut
  )
where

import           Protolude
import           Control.Arrow                  ( (***) )
-- TODO: See if we can get rid of this reference by changing Response ByteString somehow
import qualified Data.ByteString.Lazy.Char8    as L8
                                                ( ByteString )
import qualified Data.Text                     as T
import           Network.HTTP.Simple            ( Request
                                                , Response
                                                , httpLBS
                                                , setRequestMethod
                                                , parseRequest
                                                , setRequestBodyURLEncoded
                                                )
import           Web.Authenticate.OAuth         ( Credential(..)
                                                , OAuth(..)
                                                , signOAuth
                                                )

-- TODO: Build up ENV instead of just using log level
-- https://markkarpov.com/post/free-monad-considered-harmful.html
-- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
type App = ReaderT LogLevel IO
type Url = T.Text

class (Monad m) => MonadHttp m where
  makeRequest :: Request -> m (Response L8.ByteString)
  initRequest :: Url -> T.Text -> m Request
  initAuthPut :: Url -> [(T.Text, T.Text)] -> m Request

-- TODO: examine https://github.com/kazu-yamamoto/logger
data LogLevel = Debug | Info | Warn | Error | Fatal deriving (Eq, Ord, Show)
class (Monad m) => MonadLogger m where
  debug :: T.Text -> m ()
  info :: T.Text -> m ()
  warn :: T.Text -> m ()
  error :: T.Text -> m ()
  fatal :: T.Text -> m ()

class (Monad m) => MonadFile m where
  readFile' :: T.Text -> m T.Text

class (Monad m) => MonadSign m where
  sign :: OAuth -> Credential -> Request -> m Request

instance MonadHttp App where
  makeRequest = httpLBS
-- TODO: I don't like how these methods are included in MonadHttp just to avoid MonadThrow everywhere
  initRequest url verb = setRequestMethod (toS verb) <$> parseRequest (toS url)
  initAuthPut url body =
-- setRequestBodyURLEncoded automatically sets request method to "POST", so we need to set it back to put after
    setRequestMethod "PUT"
      .   setRequestBodyURLEncoded (fmap (join (***) toS) body) -- maps over list and maps over tuple
      <$> parseRequest (toS url)

-- TODO: See if we can scope this to only the level instaed of the whole ENV
instance MonadLogger App where
  debug = privateLog Debug
  info  = privateLog Info
  warn  = privateLog Warn
  error = privateLog Error
-- TODO: Look into how to kill the app (hopefully without needing more effects than ReaderT r IO
-- maybe a custom exception that goes to the top and is handled in main?)
  fatal = privateLog Fatal

privateLog :: LogLevel -> T.Text -> App ()
privateLog level msg = do
  configLevel <- ask
  when (level >= configLevel) $ putText msg

instance MonadFile App where
  readFile' path = liftIO $ readFile (toS path)
instance MonadSign App where
  sign = signOAuth

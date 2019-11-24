{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Global
  ( Url
  , LogLevel(..)
  , Config(..)
  , AppConfig(..)
  , UserConfig(..)
  , App
  , Env(..)
  , Env'(..)
  , Services(..)
  , Services'(..)
  , Log(..)
  , Log'(..)
  , Http(..)
  , Http'(..)
  , FileReader(..)
  , FileReader'(..)
  , Signer(..)
  , Signer'(..)
  , AppM(..)
  )
where

import           Protolude
import           Control.Arrow                  ( (***) )
-- TODO: See if we can get rid of this reference by changing Response ByteString somehow
import qualified Data.ByteString.Lazy.Char8    as L8
                                                ( ByteString )
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
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader
                                                )
import           Control.Monad.Catch            ( MonadThrow )
import           Data.Functor                   ( Functor )
import           Control.Applicative            ( Applicative )
import           Control.Monad                  ( Monad )
import           Control.Monad.IO.Class         ( MonadIO )

-- TODO: Build up ENV instead of just using log level
-- https://markkarpov.com/post/free-monad-considered-harmful.html
-- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
-- TODO: Figure out how to have an ENV for the whole app but restrict
-- on functions that only need part of ENV to operate.
type App = ReaderT Env IO

newtype AppM a = AppM
  { runAppM :: ReaderT Env' IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader Env')

data Env = Env
  { config :: Config
  , services :: Services
  }

data Env' = Env'
  { config' :: Config
  , services' :: Services'
  }

data Services = Services
  { logging :: Log
  , http :: Http
  , fileReader :: FileReader
  , signer :: Signer }

data Services' = Services'
  { logging' :: Log'
  , http' :: Http'
  , fileReader' :: FileReader'
  , signer' :: Signer' }

data Config = Config
  { appConfig  :: AppConfig
  , userConfig :: UserConfig
  } deriving (Eq, Show)

data AppConfig = AppConfig
  { rootUrl     :: Text
  , environment :: Text
  , platform    :: Text
  , peKey       :: Text
  , logLevel    :: LogLevel
  } deriving (Eq, Show)

data UserConfig = UserConfig
  { email      :: Text
  , password   :: Text
  , deviceGuid :: Text
  } deriving (Eq, Show)

-- TODO: examine https://github.com/kazu-yamamoto/logger
data LogLevel = Debug | Info | Warn | Error | Fatal deriving (Eq, Ord, Show)

-- TODO: Additional log levels
-- TODO: Figure out how appropriately to kill the app with fatal
data Log = Log
  { lDebug :: Text -> IO ()
  , lInfo :: Text -> IO () }
data Log' = Log'
  { lDebug' :: Text -> AppM ()
  , lInfo' :: Text -> AppM () }

type Url = Text

data Http = Http { hMakeRequest :: Request -> App (Response L8.ByteString) }
data Http' = Http' { hMakeRequest' :: Request -> AppM (Response L8.ByteString) }

data FileReader = FileReader { fReadFile :: Text -> App Text }
data FileReader' = FileReader' { fReadFile' :: Text -> AppM Text }

data Signer = Signer { sSign :: OAuth -> Credential -> Request -> App Request }
data Signer' = Signer' { sSign' :: OAuth -> Credential -> Request -> AppM Request }

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Types.Global
  ( Url
  , MonadHttp
  , MonadThrowHttp
  , MonadWriteThrowHttp
  , MonadErrorWriteThrowHttp
  , makeRequest
  , EWIO
  , WIO
  ) where

import           Control.Monad.Catch        (MonadCatch, MonadThrow)
import           Control.Monad.Except       (ExceptT, MonadError)
import           Control.Monad.Writer       (MonadWriter, WriterT)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Network.HTTP.Simple        (Request, Response, httpLBS)
import           Types.Exceptions           (CustomException (..))

type Url = String

-- TODO: There may be better ways to do this
-- https://chrispenner.ca/posts/monadio-considered-harmful
class MonadHttp m where
  makeRequest :: Request -> m (Response L8.ByteString)
instance MonadHttp IO where
  makeRequest = httpLBS
class (MonadHttp m, MonadThrow m) => MonadThrowHttp m
instance MonadThrowHttp IO

type WIO = WriterT L8.ByteString IO
instance MonadHttp WIO where
  makeRequest = httpLBS
instance MonadThrowHttp WIO
class (MonadWriter L8.ByteString m, MonadThrowHttp m) => MonadWriteThrowHttp m
instance MonadWriteThrowHttp WIO

type EWIO = ExceptT CustomException WIO

instance MonadHttp EWIO where
  makeRequest = httpLBS
instance MonadThrowHttp EWIO
instance MonadWriteThrowHttp EWIO
class (MonadWriteThrowHttp m
  , MonadError CustomException m
  , MonadCatch m) => MonadErrorWriteThrowHttp m
instance MonadErrorWriteThrowHttp EWIO

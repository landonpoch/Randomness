{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Utils.TracedErrFetch
  ( EWIO
  , request
  , jsonRequest
  ) where

import           Control.Monad.Catch        (try)
import           Control.Monad.Except       (throwError)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Types.Exceptions           (CustomException (..))
import           Types.Global               (EWIO, MonadErrorWriteThrowHttp,
                                             Url)
import qualified Utils.TracedFetch          as TF (request)

jsonRequest :: (MonadErrorWriteThrowHttp m, FromJSON a) => Url -> m a
jsonRequest url = do
  response <- request url
  case eitherDecode response of
       (Left err) -> throwError $ JsonParseError err
       (Right a)  -> return a

request :: (MonadErrorWriteThrowHttp m) => Url -> m L8.ByteString
request url = do
  resp <- try $ TF.request url
  case resp of
    (Left err)  -> throwError (err :: CustomException)
    (Right val) -> return val

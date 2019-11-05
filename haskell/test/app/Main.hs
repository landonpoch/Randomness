{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude
import           App.Bootstrapper               ( bootstrap )
import           Control.Exception              ( SomeException
                                                , catch
                                                , try
                                                )
import qualified Types.Config                  as TC
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( LogLevel(..) )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )


main = run

run :: IO ()
run = catch
  (do
    output <- try (runReaderT execApp Debug)
    case output of
      Left ex -> do
        putText "Exception:"
        -- Could just print the exception but breaking this out simply to learn
        case (ex :: CustomException) of
          KeyNotFoundError  msg  -> print ex
          JsonParseError    msg  -> print ex
          HttpBadStatusCode code -> print ex
          RandomException        -> print ex
          CryptoException msg    -> print ex
      Right r -> do
        putText "Result:"
        putText r
  )
  -- TODO: Catches all exceptions, this might be bad
  (\e -> print (e :: SomeException))

execApp :: ReaderT LogLevel IO Text
execApp = do
  config <- TC.parseConfig -- TODO: Handle exception here
  bootstrap config

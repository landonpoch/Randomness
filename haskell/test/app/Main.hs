{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
    ( ($),
      Ord((>=)),
      IO,
      Either(Right, Left),
      Text,
      try,
      when,
      catch,
      print,
      toS,
      putText,
      readFile,
      MonadIO(..),
      SomeException,
      ReaderT(runReaderT) )
import           App.Bootstrapper               ( bootstrap )
import qualified Types.Config                  as TC
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( Config(..)
                                                , AppConfig(..)
                                                , LogLevel(..)
                                                , AppM(..)
                                                , Env(..)
                                                , Services(..)
                                                , Log(..)
                                                , Http(..)
                                                , FileReader(..)
                                                , Signer(..)
                                                )
import           Network.HTTP.Simple            ( httpLBS )
import           Web.Authenticate.OAuth         ( signOAuth )

main = run

run :: IO ()
run = catch
  (do
    output <- try
      (do
        allConfig <- TC.parseConfig
        runReaderT (runAppM bootstrap) (buildEnv allConfig)
      )
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

buildEnv :: Config -> Env
buildEnv allConfig =
  Env { config = allConfig, services = buildServices $ appConfig allConfig }

readFile' :: MonadIO m => Text -> m Text
readFile' path = liftIO (readFile $ toS path)

buildServices :: AppConfig -> Services
buildServices appConfig = Services
  { logging    = buildLogger $ logLevel appConfig
  , http       = Http { makeRequest = httpLBS }
  , fileReader = FileReader { read = readFile' }
  , signer     = Signer { sign = signOAuth }
  }

output :: MonadIO m => LogLevel -> LogLevel -> Text -> m ()
output msgLevel appLevel msg = when (msgLevel >= appLevel) $ putText msg

buildLogger :: LogLevel -> Log
buildLogger appLogLevel = Log { debug = output Debug appLogLevel
                              , info  = output Info appLogLevel
                              , warn  = output Warn appLogLevel
                              , error = output Error appLogLevel
                              , fatal = output Fatal appLogLevel
                              }

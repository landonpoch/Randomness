{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude
import           App.Bootstrapper               ( bootstrap
                                                , bootstrap'
                                                )
import           Control.Exception              ( SomeException
                                                , catch
                                                , try
                                                )
import qualified Types.Config                  as TC
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( Config(..)
                                                , LogLevel(..)
                                                , App(..)
                                                , AppM(..)
                                                , appConfig
                                                , logLevel
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
                                                , Url
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )
import           Control.Monad.Catch            ( MonadThrow )
import           Network.HTTP.Simple            ( httpLBS
                                                , setRequestMethod
                                                , Request
                                                , parseRequest
                                                , setRequestBodyURLEncoded
                                                )
import           Web.Authenticate.OAuth         ( Credential(..)
                                                , OAuth(..)
                                                , signOAuth
                                                )

main = run

run :: IO ()
run = catch
  (do
    output <- try
      (do
        env <- buildEnv'
        runReaderT (runAppM bleh) env
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

output :: MonadIO m => LogLevel -> LogLevel -> Text -> m ()
output msgLevel appLevel msg = when (msgLevel >= appLevel) $ putText msg

fReadFile'' :: Text -> App Text
fReadFile'' path = lift (readFile (toS path))

fReadFile''' :: MonadIO m => Text -> m Text
fReadFile''' path = liftIO (readFile $ toS path)

buildEnv :: IO Env
buildEnv = do
  allConfig <- TC.parseConfig
  let appLogLevel = logLevel $ appConfig allConfig
  return Env
    { config   = allConfig
    , services = Services
                   { logging    = Log { lDebug = output Debug appLogLevel
                                      , lInfo  = output Info appLogLevel
                                      }
                   , http       = Http { hMakeRequest = httpLBS }
                   , fileReader = FileReader { fReadFile = fReadFile'' }
                   , signer     = Signer { sSign = signOAuth }
                   }
    }

buildEnv' :: IO Env'
buildEnv' = do
  allConfig <- TC.parseConfig
  let appLogLevel = logLevel $ appConfig allConfig
  return Env'
    { config'   = allConfig
    , services' = Services'
                    { logging'    = Log' { lDebug' = output Debug appLogLevel
                                         , lInfo'  = output Info appLogLevel
                                         }
                    , http'       = Http' { hMakeRequest' = httpLBS }
                    , fileReader' = FileReader' { fReadFile' = fReadFile''' }
                    , signer'     = Signer' { sSign' = signOAuth }
                    }
    }

bleh :: AppM Text
bleh = do
  env <- ask
  bootstrap' $ config' env

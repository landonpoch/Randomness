module Main where

import           App.SimpleBootstrapper     (bootstrap)
import qualified App.TracedBootstrapper     as T (bootstrap)
import qualified App.TracedErrBootstrapper  as TE (bootstrap)
import           Control.Exception          (SomeException, try)
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.Writer       (runWriterT)
import qualified Data.ByteString.Lazy.Char8 as L8 (putStrLn)
import           Random.Stuff               (asciiToDecimal, jsonTest)
import           Types.Config               (Config (..))
import           Types.Exceptions           (CustomException (..))

appConfig = Config { rootUrl     = "https://webapp.movetv.com/npv/cfdir.json"
                   , environment = "beta"
                   , platform    = "browser"
                   }
simple :: IO ()
simple = do
  output <- try $ bootstrap appConfig
  case output of
    Left ex ->
      case (ex :: CustomException) of
        -- TODO: Could just print the exception but breaking this out to see how
        KeyNotFoundError msg   -> print ex
        JsonParseError msg     -> print ex
        HttpBadStatusCode code -> print ex
    Right r -> do
      putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
      putStrLn "result:"
      L8.putStrLn r
      putStrLn "~~~~~~~~~~~~~~~~~~~~~~"

traced :: IO ()
traced = do
  output <- runWriterT $ T.bootstrap appConfig
  let result = fst output
  let writer = snd output
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "result:"
  L8.putStrLn result
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "writer:"
  L8.putStrLn writer
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"

tracedErr :: IO ()
tracedErr = do
  output <- runWriterT $ runExceptT $ TE.bootstrap appConfig
  let result = fst output
  let writer = snd output
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  case result of
    Left x  -> do
      putStrLn "error:"
      print (x :: CustomException)
    Right x -> do
      putStrLn "result:"
      L8.putStrLn x
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "writer:"
  L8.putStrLn writer
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"


main :: IO ()
main = do
  putStrLn "##################################################################"
  -- TODO: Probably better as a catch where we can put the entire IO action into it
  result <- try simple
  case result of
    Left x  -> print (x :: SomeException)
    Right x -> return x
  putStrLn "##################################################################"
  traced
  putStrLn "##################################################################"
  tracedErr
  putStrLn "##################################################################"
  -- val <- try $ try $ gEnvironments $ rootUrl appConfig
  -- case val of
  --   Left x -> do
  --     traceIO "err"
  --     print (x :: MyException)
  --   Right r -> do
  --     traceIO "no MyException"
  --     case r of
  --       Left rx  -> do
  --         traceIO "MyHttpException occurred"
  --         print (rx :: MyHttpException)
  --       Right rr -> print rr
  jsonTest
  print $ asciiToDecimal "-54.234"

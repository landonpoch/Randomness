module Main where

import           App.SimpleBootstrapper     (bootstrap)
import qualified App.TracedBootstrapper     as T (bootstrap)
import qualified App.TracedErrBootstrapper  as TE (bootstrap)
import           Control.Exception          (try)
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.Writer       (runWriterT)
import qualified Data.ByteString.Lazy.Char8 as L8 (putStrLn)
import           Types.Config               (Config (Config), environment,
                                             platform, rootUrl)
import           Types.Exceptions           (CustomException)

appConfig = Config { rootUrl     = "https://webapp.movetv.com/npv/cfdir.json"
                   , environment = "beta"
                   , platform    = "browser"
                   }
simple :: IO ()
simple = do
  stuff <- bootstrap appConfig
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "result:"
  L8.putStrLn stuff
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"

traced :: IO ()
traced = do
  stuff <- runWriterT $ T.bootstrap appConfig
  let result = fst stuff
  let writer = snd stuff
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "result:"
  L8.putStrLn result
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "writer:"
  L8.putStrLn writer
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"

tracedErr :: IO ()
tracedErr = do
  stuff <- runWriterT $ runExceptT $ TE.bootstrap appConfig
  let result = fst stuff
  let writer = snd stuff
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
  simple
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

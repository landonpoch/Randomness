module Main where

import           App.SimpleBootstrapper     (bootstrap)
import qualified App.TracedBootstrapper     as T (bootstrap)
import qualified App.TracedErrBootstrapper  as TE (bootstrap)
import           Control.Exception          (SomeException, catch, try)
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.Writer       (runWriterT)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, putStrLn)
import           Random.Stuff               (asciiToDecimal, jsonTest)
import           Types.Config               (Config (..))
import           Types.Exceptions           (CustomException (..))

appConfig = Config { rootUrl     = "https://webapp.movetv.com/npv/cfdir.json"
                   , environment = "beta"
                   , platform    = "browser"
                   }

main :: IO ()
main = catch
  (do
    putStrLn "###############################################################"
    simple
    putStrLn "###############################################################"
    traced
    putStrLn "###############################################################"
    tracedErr
    putStrLn "###############################################################"
    jsonTest
    print $ asciiToDecimal "-54.234"
  )
  (\e -> print (e :: SomeException))

simple :: IO ()
simple = do
  output <- try $ bootstrap appConfig
  case output of
    Left ex -> writeException ex
    Right r -> writeResult r

traced :: IO ()
traced = do
  output <- try $ runWriterT $ T.bootstrap appConfig
  case output of
    Left ex -> writeException ex
    Right (result, writer) -> do
      writeResult result
      writeWriter writer

tracedErr :: IO ()
tracedErr = do
  (result, writer) <- runWriterT $ runExceptT $ TE.bootstrap appConfig
  case result of
    Left x  -> writeException x
    Right x -> writeResult x
  writeWriter writer

writeResult :: L8.ByteString -> IO()
writeResult result = writeItem "result" $ L8.putStrLn result

writeWriter :: L8.ByteString -> IO()
writeWriter writer = writeItem "writer" $ L8.putStrLn writer

-- TODO: Could just print the exception but breaking this out to see how
writeException :: CustomException -> IO()
writeException ex =
  writeItem "error" $
    case (ex :: CustomException) of
      KeyNotFoundError msg   -> print ex
      JsonParseError msg     -> print ex
      HttpBadStatusCode code -> print ex

writeItem :: String -> IO() -> IO()
writeItem item write = do
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn $ item ++ ":"
  write
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn ""

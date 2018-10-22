module Main where

import           App.Bootstrapper           (bootstrap)
import           Control.Exception          (SomeException, catch, try)
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
    output <- try $ bootstrap appConfig

    putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
    case output of
      Left ex -> do
        putStrLn "Exception:"
        -- Could just print the exception but breaking this out simply to learn
        case (ex :: CustomException) of
          KeyNotFoundError msg   -> print ex
          JsonParseError msg     -> print ex
          HttpBadStatusCode code -> print ex
          RandomException        -> print ex
      Right r -> do
        putStrLn "Result:"
        L8.putStrLn r
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn ""

    -- putStrLn "#############################################################"
    -- jsonTest
    -- print $ asciiToDecimal "-54.234"
  )
  -- TODO: Catches all exceptions, this might be bad
  (\e -> print (e :: SomeException))

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App.Bootstrapper               ( bootstrap )
import           Control.Exception              ( SomeException
                                                , catch
                                                , try
                                                )
import qualified Data.ByteString.Lazy.Char8    as L8
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Types.Config                  as TC
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( trace )
-- import           Random.Stuff               (asciiToDecimal, jsonTest)

main = run

run :: IO ()
run = catch
  (do
    config <- TC.parseConfig -- TODO: Handle exception here
    output <- try $ bootstrap config

    TIO.putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
    case output of
      Left ex -> do
        TIO.putStrLn "Exception:"
        -- Could just print the exception but breaking this out simply to learn
        case (ex :: CustomException) of
          KeyNotFoundError  msg  -> print ex
          JsonParseError    msg  -> print ex
          HttpBadStatusCode code -> print ex
          RandomException        -> print ex
          CryptoException msg    -> print ex
      Right r -> do
        TIO.putStrLn ""
        -- TIO.putStrLn "Result:"
        -- TIO.putStrLn r
    TIO.putStrLn "~~~~~~~~~~~~~~~~~~~~~~"
    TIO.putStrLn ""

    -- putStrLn "#############################################################"
    -- jsonTest
    -- print $ asciiToDecimal "-54.234"
  )
  -- TODO: Catches all exceptions, this might be bad
  (\e -> print (e :: SomeException))

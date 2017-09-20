{-# OPTIONS_GHC -funbox-strict-fields #-}  -- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy.Internal as I
import Data.Text as Text
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import GHC.Exts
import Lib
import Data.Scientific as Scientific

main :: IO ()
main = do
    let encoded = encode ([1,2,3] :: [Int])
    prettyPrint encoded
    print (decode encoded :: Maybe [Int])
    print (eitherDecode encoded :: Either String [Int])
    let val = (Object $ fromList
                  [ ("numbers", Array $ fromList [Number 1, Number 2, Number 3])
                  , ("test", Bool True)
                  ] :: Value)
    let encodedObj = encode val
    prettyPrint encodedObj
    print (eitherDecode encodedObj :: Either String TestObj)
    let encodedPerson = encode Person { name = "Test Person", age=16 }
    prettyPrint encodedPerson
    print (eitherDecode encodedPerson :: Either String Person)
    let people = [ Person { name = "Test Person 1", age = 16 }
                 , Person { name = "Test Person 2", age = 15 }
                 ]
    let encodedPeople = (encode people)
    prettyPrint encodedPeople
    print (eitherDecode encodedPeople :: Either String [Person])
    let prims = [PrimInt 4, PrimString "hello!"]
    let encodedPrims = encode prims
    prettyPrint encodedPrims
    print (eitherDecode encodedPrims :: Either String [Prim])
    putStrLn "end!"

prettyPrint :: I.ByteString -> IO()
prettyPrint = T.putStrLn . T.decodeUtf8

data Prim = PrimInt Int | PrimString String deriving Show

toInt :: Scientific -> Int
toInt x = (\(Right y) -> y) (floatingOrInteger x)

instance FromJSON Prim where
    parseJSON (String x) = return $ PrimString $ unpack x
    parseJSON (Number x) = return $ PrimInt $ toInt x
    parseJSON _ = fail "Unsuppored array item"

instance ToJSON Prim where
    toJSON (PrimInt x)    = toJSON x
    toJSON (PrimString x) = toJSON x

data TestObj = TestObj
    { test    :: !Bool
    , numbers :: ![Int]
    } deriving Show

instance FromJSON TestObj where
    parseJSON = withObject "testObj" $ \o -> do
        test <- o .: "test"
        numbers <- o .: "numbers"
        return TestObj{..}

data Person = Person
    { name :: !String
    , age  :: !Int
    } deriving Show

instance FromJSON Person where
    parseJSON = withObject "person" $ \o -> do
        name <- o .: "name"
        age <- o .: "age"
        return Person{..}

instance ToJSON Person where
    toJSON Person{..} = object [
        "name" .= name, 
        "age" .= age]
{-# OPTIONS_GHC -funbox-strict-fields #-}  -- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy.Internal as I
import Data.Text as Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Exts
import Lib
import Data.Scientific as Scientific
import qualified Data.HashMap.Lazy as HML        ( member )

main :: IO ()
main = do
    print $ stringToInteger "1035"

stringToInteger :: String -> Int
stringToInteger s = sum $ Prelude.zipWith (*)
                    (powersOfTen 0) -- Initial number should be less than zero if floating point required
                    (Prelude.map charToInt $ Prelude.reverse s)

powersOfTen :: Int -> [Int]
powersOfTen initialNumber = Prelude.map (\x -> 10 ^ x) [initialNumber..]

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
    
json :: IO()
json = do
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
    print (eitherDecode "[4.1]" :: Either String [Prim])
    let things = [ Thing { description = "Crap", quantity = 1 }]
    let encodedThings = encode things
    prettyPrint encodedThings
    print (eitherDecode encodedThings :: Either String [Thing])
    print (eitherDecode encodedThings :: Either String [Container])
    print (eitherDecode encodedPeople :: Either String [Container])
    let heteros = [ ThingContainer Thing { description = "Crap", quantity = 1 }
                  , PersonContainer Person { name = "Landon", age = 40 }
                  ]
    let encodedHeteros = encode heteros
    prettyPrint encodedHeteros
    print (eitherDecode encodedHeteros :: Either String [Container])
    putStrLn "end!"

prettyPrint :: I.ByteString -> IO()
prettyPrint = T.putStrLn . T.decodeUtf8

data Prim = PrimInt Int | PrimString String deriving Show
data Container = PersonContainer Person | ThingContainer Thing deriving Show

toInt :: Scientific -> Maybe Int
toInt = forceInt . floatingOrInteger
    where forceInt (Left _)  = Nothing
          forceInt (Right x) = Just x

instance FromJSON Prim where
    parseJSON (String x) = return . PrimString . unpack $ x
    parseJSON (Number x) = tryInt . toInt $ x
        where tryInt (Nothing) = fail "Unsupported array item"
              tryInt (Just y)  = return . PrimInt $ y
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
        test    <- o .: "test"
        numbers <- o .: "numbers"
        return TestObj{..}

data Person = Person
    { name :: !String
    , age  :: !Int
    } deriving Show

instance FromJSON Person where
    parseJSON = withObject "person" $ \o -> do
        name <- o .: "name"
        age  <- o .: "age"
        return Person{..}

instance ToJSON Person where
    toJSON Person{..} = object [
        "name" .= name, 
        "age"  .= age]

data Thing = Thing
    { description :: !String
    , quantity    :: !Int
    } deriving Show

instance FromJSON Thing where
    parseJSON = withObject "thing" $ \o -> do
        description <- o .: "description"
        quantity    <- o .: "quantity"
        return Thing{..}

instance ToJSON Thing where
    toJSON Thing{..} = object [
        "description" .= description
        , "quantity"  .= quantity ]

instance FromJSON Container where
    parseJSON (Object o) = if HML.member "name" o
        then fmap PersonContainer $ (parseJSON :: Value -> Parser Person) . Object $ o
        else fmap ThingContainer $ (parseJSON :: Value -> Parser Thing) . Object $ o

instance ToJSON Container where
    toJSON (ThingContainer x)  = toJSON x
    toJSON (PersonContainer x) = toJSON x
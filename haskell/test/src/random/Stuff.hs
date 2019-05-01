{-# OPTIONS_GHC -funbox-strict-fields #-}  -- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Random.Stuff
    ( asciiToDecimal
    , jsonTest
    )
where

import           Control.Monad                  ( fail )
import           Protolude
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Internal as I
import qualified Data.HashMap.Lazy             as HML
                                                ( member )
import qualified Data.List                     as L
                                         hiding ( filter
                                                , foldl
                                                , head
                                                , map
                                                , zipWith
                                                )
import qualified Data.Maybe                    as M
import           Data.Scientific               as Scientific
import           Data.Text                     as Text
                                         hiding ( filter
                                                , foldl
                                                , head
                                                , map
                                                , zipWith
                                                )
import qualified Data.Text.Lazy.Encoding       as T
import qualified Data.Text.Lazy.IO             as T
import           GHC.Exts

asciiToDecimal :: Text.Text -> Double
asciiToDecimal t =
    let s          = Text.unpack t
        characters = "0123456789"
        reversed   = foldl (flip (:)) [] s
    in  (if (M.fromMaybe '0' (head s)) == '-' then negate else identity)
            . sum
            $ zipWith
                  (*)
                  ( map fromIntegral
                  . M.mapMaybe (`L.elemIndex` characters)
                  $ filter (`elem` characters) reversed
                  )
                  (map
                      (10.0 ^^)
                      [negate $ M.fromMaybe 0 (L.elemIndex '.' reversed) ..]
                  )

jsonTest :: IO ()
jsonTest = do
    let encoded = encode ([1, 2, 3] :: [Int])
    prettyPrint encoded
    print (decode encoded :: Maybe [Int])
    print (eitherDecode encoded :: Either [Char] [Int]) -- TODO: See if there is a better alternative to [Char] here
    let val =
            Object $ fromList
                [ ("numbers", Array $ fromList [Number 1, Number 2, Number 3])
                , ("test"   , Bool True)
                ] :: Value
    let encodedObj = encode val
    prettyPrint encodedObj
    print (eitherDecode encodedObj :: Either [Char] TestObj)
    let encodedPerson = encode Person { name = "Test Person", age = 16 }
    prettyPrint encodedPerson
    print (eitherDecode encodedPerson :: Either [Char] Person)
    let people =
            [ Person { name = "Test Person 1", age = 16 }
            , Person { name = "Test Person 2", age = 15 }
            ]
    let encodedPeople = encode people
    prettyPrint encodedPeople
    print (eitherDecode encodedPeople :: Either [Char] [Person])
    let prims        = [PrimInt 4, PrimString "hello!"]
    let encodedPrims = encode prims
    prettyPrint encodedPrims
    print (eitherDecode encodedPrims :: Either [Char] [Prim])
    print (eitherDecode "[4.1]" :: Either [Char] [Prim])
    let things = [Thing { description = "Crap", quantity = 1 }]
    let encodedThings = encode things
    prettyPrint encodedThings
    print (eitherDecode encodedThings :: Either [Char] [Thing])
    print (eitherDecode encodedThings :: Either [Char] [Container])
    print (eitherDecode encodedPeople :: Either [Char] [Container])
    let heteros =
            [ ThingContainer Thing { description = "Crap", quantity = 1 }
            , PersonContainer Person { name = "Landon", age = 40 }
            ]
    let encodedHeteros = encode heteros
    prettyPrint encodedHeteros
    print (eitherDecode encodedHeteros :: Either [Char] [Container])
    print "end!"

prettyPrint :: I.ByteString -> IO ()
prettyPrint = T.putStrLn . T.decodeUtf8

data Prim = PrimInt Int | PrimString Text.Text deriving Show
data Container = PersonContainer Person | ThingContainer Thing deriving Show

toInt :: Scientific -> Maybe Int
toInt = forceInt . floatingOrInteger
  where
    forceInt (Left  _) = Nothing
    forceInt (Right x) = Just x

instance FromJSON Prim where
    parseJSON (String x) = return $ PrimString x
    parseJSON (Number x) = tryInt . toInt $ x
      where
        tryInt Nothing  = fail "Unsupported array item"
        tryInt (Just y) = return . PrimInt $ y
    parseJSON _ = fail "Unsuppored array item"

instance ToJSON Prim where
    toJSON (PrimInt    x) = toJSON x
    toJSON (PrimString x) = toJSON x

data TestObj = TestObj
    { test    :: !Bool
    , numbers :: ![Int]
    } deriving Show

instance FromJSON TestObj where
    parseJSON = withObject "testObj" $ \o -> do
        test    <- o .: "test"
        numbers <- o .: "numbers"
        return TestObj { .. }

data Person = Person
    { name :: !Text.Text
    , age  :: !Int
    } deriving Show

instance FromJSON Person where
    parseJSON = withObject "person" $ \o -> do
        name <- o .: "name"
        age  <- o .: "age"
        return Person { .. }

instance ToJSON Person where
    toJSON Person {..} = object ["name" .= name, "age" .= age]

data Thing = Thing
    { description :: !Text.Text
    , quantity    :: !Int
    } deriving Show

instance FromJSON Thing where
    parseJSON = withObject "thing" $ \o -> do
        description <- o .: "description"
        quantity    <- o .: "quantity"
        return Thing { .. }

instance ToJSON Thing where
    toJSON Thing {..} =
        object ["description" .= description, "quantity" .= quantity]

instance FromJSON Container where
    parseJSON (Object o) = if HML.member "name" o
        then
            fmap PersonContainer
            $ (parseJSON :: Value -> Parser Person)
            . Object
            $ o
        else
            fmap ThingContainer
            $ (parseJSON :: Value -> Parser Thing)
            . Object
            $ o

instance ToJSON Container where
    toJSON (ThingContainer  x) = toJSON x
    toJSON (PersonContainer x) = toJSON x

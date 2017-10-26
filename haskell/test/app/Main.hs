{-# OPTIONS_GHC -funbox-strict-fields #-}  -- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import GHC.Exts
import Lib
import Data.Scientific as Scientific
import Data.Text as Text hiding             ( map, filter, zipWith, foldl, head )
import qualified Data.HashMap.Lazy as HML   ( member )
import qualified Data.HashMap.Strict as HMS ( lookup )
import qualified Data.List as L hiding      ( map, filter, zipWith, foldl, head )
import qualified Data.Maybe as M
import qualified Types.Environments as T
import qualified Types.Hostnames as TH
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy.Internal as I
import qualified Data.ByteString.Lazy.Char8 as L8 ( ByteString, putStrLn )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import Text.Printf (printf)
import Control.Monad.Writer

data Config = Config
    { rootUrl     :: !String
    , environment :: !String
    , platform    :: !String
    } deriving Show

main :: IO ()
main = do
    let bleh = LBS.toChunks "This is a test"
    let bleh2 = SBS.unpack "This is another test"
    let bleh3 = "This is yet another test" :: SBS.ByteString
    let bleh4 = "This is a fourth test" :: LBS.ByteString
    let bleh5 = "This is a fifth test"
    putStrLn "test"

random :: IO ()
random = do
    resp <- writerRequest "GET https://webapp.movetv.com/npv/cfdir.json"
    let val = runWriter (resp :: Writer [L8.ByteString] (Either String T.Environments))
    mapM_ print . fst $ val
    mapM_ L8.putStrLn . snd $ val
    -- mapM_ putStrLn $ snd $ runWriter $ gcd' 2030402 30408

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with value " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " `mod` " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

run :: IO ()
run = do
    let appConfig = Config { rootUrl     = "GET https://webapp.movetv.com/npv/cfdir.json"
                           , environment = "beta"
                           , platform    = "browser"
                           }
    response <- (Lib.requestJSON $ rootUrl appConfig) :: IO (Either String T.Environments)
    either putStrLn (\x -> selectEnvironment appConfig x) $ response

selectEnvironment :: Config -> T.Environments -> IO()
selectEnvironment config environments = do
    let environmentValue = HMS.lookup (environment config) $ T.environments environments
    let maybeConfigHost = environmentValue >>= T.configHostSsl
    M.maybe
        (putStrLn "Unable to get environment list")
        (\configHost -> getEnvironments configHost (platform config) $ environment config)
        maybeConfigHost
--main = print $ (asciiToDecimal "-$104,689.357") * 2

getEnvironments :: String -> String -> String -> IO ()
getEnvironments configHost platform env = do
    let url = printf "GET %s/env-list/%s-sling.json" configHost $ platform
    hostnames <- (Lib.requestJSON url) :: IO (Either String TH.HostnameEnvironments)
    either putStrLn (\x -> getPeFile platform env x) $ hostnames

getPeFile :: String -> String -> TH.HostnameEnvironments -> IO ()
getPeFile platform env hostnames = do
    let selectedHostnames = HMS.lookup env (TH.environments (hostnames :: TH.HostnameEnvironments))
    M.maybe
        (putStrLn "No ums endpoint found")
        (\x -> do
            let peUrl = (printf "GET %s/%s/sling/pe-%s.xml.enc" (TH.appCastUrl x) platform env)
            Lib.printRequest peUrl)
        selectedHostnames

-- authenticate :: String -> IO ()
-- authenticate = Lib.printRequest

asciiToDecimal :: String -> Double
asciiToDecimal s = 
    let characters = "0123456789"
        reversed   = foldl (\acc x -> x:acc) [] s
    in (if head s == '-' then negate else id) . sum $ zipWith (*)
        (map fromIntegral . M.catMaybes . map (`L.elemIndex` characters) $ filter (`elem` characters) reversed)
        (map (10.0^^) [negate $ M.fromMaybe 0 (L.elemIndex '.' reversed)..])
    
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
{-# OPTIONS_GHC -funbox-strict-fields #-}  -- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad.Except          (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString               as SBS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as L8 (ByteString, putStrLn)
import qualified Data.ByteString.Lazy.Internal as I
import qualified Data.HashMap.Lazy             as HML (member)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HMS (lookup)
import qualified Data.List                     as L hiding (filter, foldl, head,
                                                     map, zipWith)
import qualified Data.Maybe                    as M
import           Data.Scientific               as Scientific
import           Data.Text                     as Text hiding (filter, foldl,
                                                        head, map, zipWith)
import qualified Data.Text.Lazy.Encoding       as T
import qualified Data.Text.Lazy.IO             as T
import           GHC.Exts
import           Lib
import           Text.Printf                   (printf)
import qualified Types.Environments            as T
import qualified Types.Hostnames               as TH

data Config = Config
    { rootUrl     :: !String
    , environment :: !String
    , platform    :: !String
    } deriving Show

appConfig = Config { rootUrl     = "GET https://webapp.movetv.com/npv/cfdir.json"
                   , environment = "beta"
                   , platform    = "browser"
                   }

main :: IO ()
main = do
  response <- runExceptT $ bootstrap appConfig
  case response of
       Left l  -> putStrLn l
       Right r -> L8.putStrLn r

-- TODO: see if you can reintroduce WriterT into the stack
-- TODO: Use real exception types instead of strings
-- see: https://softwareengineering.stackexchange.com/questions/252977/cleanest-way-to-report-errors-in-haskell
-- see: https://www.schoolofhaskell.com/user/commercial/content/exceptions-best-practices
bootstrap :: Config -> ExceptT String IO I.ByteString
bootstrap config = do
  let targetEnvironment = environment config
  let targetPlatform = platform config
  environments <- getEnvironments $ rootUrl config
  selectedEnv <- selectEnvironment environments targetEnvironment
  hostnamesByEnvironment <- getHostnames (T.configHost selectedEnv) targetPlatform
  selectedHostnames <- selectHostnames hostnamesByEnvironment targetEnvironment
  configHostname <- getConfigHost selectedHostnames
  liftIO $ getPeFile configHostname targetPlatform targetEnvironment

getEnvironments :: String -> ExceptT String IO T.Environments
getEnvironments = mtlRequest

selectEnvironment :: T.Environments -> String -> ExceptT String IO T.Environment
selectEnvironment environments env = do
  let maybeEnvironment = HMS.lookup env $ T.environments environments
  toExceptT maybeEnvironment "target environment doesn't exist"

getHostnames :: String -> String -> ExceptT String IO TH.HostnameEnvironments
getHostnames configHostname platform = do
  let hostnamesUrl = printf "GET %s/env-list/%s-sling.json" configHostname platform
  mtlRequest hostnamesUrl

selectHostnames :: TH.HostnameEnvironments -> String -> ExceptT String IO TH.Hostnames
selectHostnames hostnamesByEnvironment env = do
  let maybeHostnames = HMS.lookup env $ TH.environments hostnamesByEnvironment
  toExceptT maybeHostnames "environment missing hostnames"

getConfigHost :: TH.Hostnames -> ExceptT String IO String
getConfigHost selectedHostnames = toExceptT (TH.appCastUrl selectedHostnames) "config url is missing from hostnames"

getPeFile :: String -> String -> String -> IO I.ByteString
getPeFile configHost platform env = do
  let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" configHost platform env
  textRequest peUrl

toExceptT :: Maybe a -> String -> ExceptT String IO a
toExceptT m err = case m of
  Nothing  -> throwError err
  (Just x) -> return x

-- Other random testing that is unrelated
asciiToDecimal :: String -> Double
asciiToDecimal s =
    let characters = "0123456789"
        reversed   = foldl (flip (:)) [] s
    in (if head s == '-' then negate else id) . sum $ zipWith (*)
        (map fromIntegral . M.mapMaybe (`L.elemIndex` characters) $ filter (`elem` characters) reversed)
        (map (10.0^^) [negate $ M.fromMaybe 0 (L.elemIndex '.' reversed)..])

json :: IO()
json = do
    let encoded = encode ([1,2,3] :: [Int])
    prettyPrint encoded
    print (decode encoded :: Maybe [Int])
    print (eitherDecode encoded :: Either String [Int])
    let val = Object $ fromList
                  [ ("numbers", Array $ fromList [Number 1, Number 2, Number 3])
                  , ("test", Bool True)
                  ] :: Value
    let encodedObj = encode val
    prettyPrint encodedObj
    print (eitherDecode encodedObj :: Either String TestObj)
    let encodedPerson = encode Person { name = "Test Person", age=16 }
    prettyPrint encodedPerson
    print (eitherDecode encodedPerson :: Either String Person)
    let people = [ Person { name = "Test Person 1", age = 16 }
                 , Person { name = "Test Person 2", age = 15 }
                 ]
    let encodedPeople = encode people
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
        where tryInt Nothing  = fail "Unsupported array item"
              tryInt (Just y) = return . PrimInt $ y
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

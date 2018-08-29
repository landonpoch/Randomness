{-# OPTIONS_GHC -funbox-strict-fields #-}  -- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Arrow                 (right)
import           Control.Monad.Trans.Either    (EitherT, hoistEither,
                                                runEitherT)
import           Control.Monad.Trans.Maybe     (MaybeT)
import           Control.Monad.Writer
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

getEndpoints :: String -> String -> Lib.Tracer TH.HostnameEnvironments
getEndpoints configHost platform = do
  let url = printf "GET %s/env-list/%s-sling.json" configHost platform
  tracedRequest url :: Lib.Tracer TH.HostnameEnvironments

-- TODO: see if you can reintroduce WriterT into the stack
-- TODO: switch from EitherT to ExceptT and use real exception types
-- see: https://softwareengineering.stackexchange.com/questions/252977/cleanest-way-to-report-errors-in-haskell
chain :: EitherT String IO I.ByteString
chain = do
  let url = rootUrl appConfig
  environments <- plainRequest url :: EitherT String IO T.Environments
  let targetEnvironment = environment appConfig
  let desiredEnvironment = convertMaybe $ HMS.lookup targetEnvironment (T.environments environments)
  selectedEnvironment <- hoistEither desiredEnvironment
  let configHost = T.configHost selectedEnvironment
  let platform = "browser" :: String
  let nextUrl = printf "GET %s/env-list/%s-sling.json" configHost platform :: String
  hostnamesByEnvironment <- plainRequest nextUrl :: EitherT String IO TH.HostnameEnvironments
  let hmHostmaps = TH.environments hostnamesByEnvironment
  let hostnames = convertMaybe $ HMS.lookup targetEnvironment hmHostmaps
  selectedHostnames <- hoistEither hostnames
  let appCastUrl = TH.appCastUrl selectedHostnames
  let converted = convertMaybe appCastUrl
  rar <- hoistEither converted
  let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" rar platform targetEnvironment :: String
  let response = textRequest peUrl
  liftIO response

convertMaybe :: Maybe a -> Either String a
convertMaybe Nothing  = Left "desired item doesn't exist"
convertMaybe (Just x) = Right x

main :: IO ()
main = do
  -- let test = chain :: EitherT String IO TH.HostnameEnvironments
  let test = chain
  test2 <- runEitherT test
  let test3 = case test2 of
                   Left l  -> putStrLn l
                   Right r -> L8.putStrLn r
  test3
  -- let url = rootUrl appConfig
  -- let resp = tracedRequest url :: Lib.Tracer T.Environments
  -- parsedResp <- runWriterT resp
  -- L8.putStrLn (snd parsedResp)
  -- let targetEnvironment = environment appConfig
  -- let environments = T.environments <$> fst parsedResp
  -- -- let thing = case environments of
  -- --                  Left l  -> putStrLn l
  -- --                  Right r -> return r
  -- let desiredEnvironment = fmap (HMS.lookup targetEnvironment) environments
  -- let desiredConfigHost = fmap (fmap T.configHost) desiredEnvironment
  -- let hostnames = fmap (fmap (\x -> getEndpoints x "ios")) desiredConfigHost
  -- parsedHosnames <- runWriterT hostnames
  -- L8.putStrLn (snd parsedHosnames)
  -- let stuff = fmap grrr desiredEnvironment
  -- stuff <- (loggedRequest url) :: IO (Either String T.Environments)
  -- environments <- getEnvironments $ rootUrl appConfig
  -- parsed <- unwrap environments
  -- -- let tester = parsed >>= (\thing -> Right (selectEnvironment (environment appConfig)))
  -- let selectedEnvironment = right (selectEnvironment (environment appConfig)) parsed
  -- let test = right (fmap (\configHost -> getEnvironmentEndpoints configHost $ platform appConfig)) selectedEnvironment
  return ()
    -- let bleh = LBS.toChunks "This is a test"
    -- let bleh2 = SBS.unpack "This is another test"
    -- let bleh3 = "This is yet another test" :: SBS.ByteString
    -- let bleh4 = "This is a fourth test" :: LBS.ByteString
    -- let bleh5 = "This is a fifth test"
    -- putStrLn "test"

type MaybeIO a = MaybeT IO a

unwrap :: Lib.TracedRequest a -> IO(Either String a)
unwrap item = do
  let parsed = runWriter item
  mapM_ L8.putStrLn . snd $ parsed
  return . fst $ parsed

-- random :: IO ()
-- random = do
--     let appConfig = Config { rootUrl     = "GET https://webapp.movetv.com/npv/cfdir.json"
--                            , environment = "beta"
--                            , platform    = "browser"
--                            }
--     resp <- traceRequest $ rootUrl appConfig
--     let val = runWriter (resp :: Lib.TracedRequest T.Environments)
--     mapM_ L8.putStrLn . snd $ val
--     let test = fst val
--     let host = right (selectEnvironment appConfig) (fst val)
--     let envDetails = right (fmap (\h -> getEnvironments h (environment appConfig) (platform appConfig))) host
--     return ()
--     -- mapM_ print . fst $ val
--     -- mapM_ putStrLn $ snd $ runWriter $ gcd' 2030402 30408

getEnvironments :: Lib.Url -> IO (Lib.TracedRequest T.Environments)
getEnvironments = traceRequest

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with value " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " `mod` " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- run :: IO ()
-- run = do
--     let appConfig = Config { rootUrl     = "GET https://webapp.movetv.com/npv/cfdir.json"
--                            , environment = "beta"
--                            , platform    = "browser"
--                            }
--     response <- (Lib.requestJSON $ rootUrl appConfig) :: IO (Either String T.Environments)
--     either putStrLn (selectEnvironment appConfig) response

-- TODO: Unwind these calls that get made inside of calls.  They are all nested
-- and instead should be done in some sort of a controller function.
selectEnvironment :: String -> T.Environments -> Maybe String
selectEnvironment environment environments = do
    let environmentValue = HMS.lookup environment $ T.environments environments
    environmentValue >>= T.configHostSsl

--main = print $ (asciiToDecimal "-$104,689.357") * 2

getEnvironmentEndpoints :: String -> String -> IO (Lib.TracedRequest TH.HostnameEnvironments)
getEnvironmentEndpoints configHost platform = traceRequest
  $ printf "GET %s/env-list/%s-sling.json" configHost platform

getPeFile :: String -> String -> String -> IO ()
getPeFile appCastUrl platform env = Lib.printRequest
  $ printf "GET %s/%s/sling/pe-%s.xml.enc" appCastUrl platform env
    -- let selectedHostnames = HMS.lookup env (TH.environments (hostnames :: TH.HostnameEnvironments))
    -- M.maybe
    --     (putStrLn "No ums endpoint found")
    --     (\x -> do
    --         let peUrl = printf "GET %s/%s/sling/pe-%s.xml.enc" (TH.appCastUrl x) platform env
    --         Lib.printRequest peUrl)
    --     selectedHostnames

-- authenticate :: String -> IO ()
-- authenticate = Lib.printRequest

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

-- isValid :: String -> Bool
-- isValid s = L.length s >= 8
--             -- && L.any isAlpha s

-- getPassphrase :: IO (Maybe String)
-- getPassphrase = do s <- getLine
--                    if isValid s then return $ Just s
--                                 else return Nothing
--
-- askPassphrase :: IO ()
-- askPassphrase = do putStrLn "Insert your new passphrase:"
--                    maybe_value <- getPassphrase
--                    case maybe_value of
--                        Just value -> putStrLn "Storing password"
--                        Nothing    -> putStrLn "Invalid passphrase"
--
-- getPassphrase' :: Control.Monad.Trans.Maybe.MaybeT IO String
-- getPassphrase' = do

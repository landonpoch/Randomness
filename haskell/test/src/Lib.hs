module Lib
    ( requestJSON
    , printRequest
    , writerRequest
    ) where

import           Control.Monad                    ( (<=<) )
import           Control.Monad.Writer
import           Network.HTTP.Simple              ( getResponseBody
                                                  , httpJSON
                                                  , httpLBS
                                                  , parseRequest
                                                  , Request
                                                  , Response 
                                                  )
import           Data.Aeson                       ( FromJSON, eitherDecode )
import qualified Data.ByteString.Lazy.Char8 as L8 ( ByteString, putStrLn )

type Url = String
requestJSON :: (FromJSON a) => Url -> IO (Either String a)
requestJSON url = do
    putStrLn url
    let response = request httpLBS $ url
    resp <- response
    L8.putStrLn resp
    return $ eitherDecode resp

printRequest :: Url -> IO ()
printRequest url = do
    putStrLn url
    L8.putStrLn <=< request httpLBS $ url

writerRequest :: (FromJSON a) => Url -> IO (Writer [L8.ByteString] (Either String a))
writerRequest url = do
    let response = request httpLBS $ url
    resp <- response
    return $ (\x -> do
        tell [resp]
        return $ eitherDecode x) resp

request :: (Request -> IO (Response a)) -> Url -> IO (a)
request requestMechanism = fmap getResponseBody . requestMechanism <=< parseRequest
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Pe
  ( decryptPeFile
  , getSecrets
  )
where

import           Protolude
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Crypto.Cipher.AES              ( AES128 )
import           Crypto.Cipher.Types            ( BlockCipher(..)
                                                , Cipher(..)
                                                , IV
                                                , makeIV
                                                )
import           Crypto.Error                   ( CryptoFailable(..) )
import qualified Data.ByteArray.Encoding       as BAE
import qualified Data.ByteString               as BS
import           Data.Char                      ( isSpace )
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Text.XML.Light.Input           ( parseXML )
import           Text.XML.Light.Proc            ( findAttr
                                                , findElement
                                                , onlyElems
                                                )
import           Text.XML.Light.Types           ( QName(..) )
import           Types.Exceptions               ( CustomException(..) )
import           Types.Global                   ( MonadFile
                                                , readFile'
                                                )


decryptPeFile :: (MonadError CustomException m) => T.Text -> T.Text -> m T.Text
decryptPeFile keyStr msg = do
  let sanitizedMsg = T.filter (not . isSpace) msg
  let keyEncoded   = BAE.convertFromBase BAE.Base16 $ TE.encodeUtf8 keyStr
  keyHex :: BS.ByteString <- case keyEncoded of
    Left  s -> throwError $ CryptoException $ toS s
    Right r -> return r
  key :: AES128 <- case cipherInit keyHex of
    -- TODO: See if there's a way to handle the original crypto error here
    CryptoFailed e -> throwError $ CryptoException "Couldn't initialize key"
    CryptoPassed c -> return c
  let converted = BAE.convertFromBase BAE.Base64 (TE.encodeUtf8 sanitizedMsg)
  b64Str <- case converted of
    Left  s -> throwError $ CryptoException "PE file not valid Base64 string"
    Right r -> return r
  let (iv, encrypted) = splitAt 16 $ BS.unpack b64Str
  initVector :: IV AES128 <- case makeIV (BS.pack iv) of
    Nothing ->
      throwError $ CryptoException "Invalid initialization vector in PE file"
    Just j -> return j
  let decryptedMsg = cbcDecrypt key initVector (BS.pack encrypted)
  return $ toS decryptedMsg

getSecrets :: (MonadError CustomException m) => T.Text -> m (T.Text, T.Text)
getSecrets peContents = do
  let parsed   = parseXML peContents
  let elements = onlyElems parsed
  let found = fmap
        (findElement $ QName "oauth"
                             (Just "http://www.movenetworks.com/ap/pe-1.0")
                             (Just "ap")
        )
        elements
  let foundElements = catMaybes found
  element <- case head foundElements of
    Nothing -> throwError $ KeyNotFoundError "Missing oauth node in PE file"
    Just a  -> return a
  consumerKey <- case findAttr (QName "consumerKey" Nothing Nothing) element of
    Nothing -> throwError $ KeyNotFoundError "No consumer key"
    Just a  -> return $ toS a
  consumerSecret <-
    case findAttr (QName "consumerSecret" Nothing Nothing) element of
      Nothing -> throwError $ KeyNotFoundError "No consumer secret"
      Just a  -> return $ toS a
  return (consumerKey, consumerSecret)

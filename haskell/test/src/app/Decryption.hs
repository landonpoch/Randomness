{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: Move this module to a more appropriate place and consider renaming it
module App.Decryption
  ( decryptPeFile
  , getPeKey
  ) where

import           Control.Monad.Except    (MonadError, throwError)
import           Crypto.Cipher.AES       (AES128)
import           Crypto.Cipher.Types     (BlockCipher(..), Cipher(..), IV, makeIV)
import           Crypto.Error            (CryptoFailable(..))
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString         as BS
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Types.Exceptions        (CustomException(..))
import           Data.Char               (isSpace)
import Types.Global (MonadFile, readFile')

decryptPeFile :: (MonadError CustomException m) => T.Text -> T.Text -> m T.Text
decryptPeFile keyStr msg = do
  let sanitizedMsg = T.filter (not . isSpace) msg
  let keyEncoded = BAE.convertFromBase BAE.Base16 $ TE.encodeUtf8 keyStr
  keyHex :: BS.ByteString <- case keyEncoded of
    Left s  -> throwError $ CryptoException $ T.pack s
    Right r -> return r
  key :: AES128 <- case cipherInit keyHex of
    -- TODO: See if there's a way to handle the original crypto error here
    CryptoFailed e -> throwError $ CryptoException "Couldn't initialize key"
    CryptoPassed c -> return c
  let converted = BAE.convertFromBase BAE.Base64 (TE.encodeUtf8 sanitizedMsg)
  b64Str <- case converted of
    Left s  -> throwError $ CryptoException "PE file not valid Base64 string"
    Right r -> return r
  let (iv, encrypted) = splitAt 16 $ BS.unpack b64Str
  initVector :: IV AES128 <- case makeIV (BS.pack iv) of
    Nothing -> throwError $ CryptoException "Invalid initialization vector in PE file"
    Just j  -> return j
  let decryptedMsg = cbcDecrypt key initVector (BS.pack encrypted)
  return $ TE.decodeUtf8 decryptedMsg

getPeKey :: (MonadFile m) => m T.Text
getPeKey = do
  key <- readFile' "pekey.txt"
  let sanitizedKey = T.filter (not . isSpace) key
  return sanitizedKey

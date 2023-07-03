module YIBP.Crypto (encrypt, decrypt, encryptRandom) where

import YIBP.Crypto.TH

import Data.ByteString qualified as BS

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, makeIV)
import Crypto.Error
import Crypto.Random.Types qualified as CRT

cryptoKey :: BS.ByteString
cryptoKey = $(cryptoKeyTH "extra/random-key.default")

cryptoCipher :: AES256
cryptoCipher = case cipherInit cryptoKey of
  CryptoFailed err -> error $ "unexpected error: Unable to initialize cipher: " <> show err
  CryptoPassed x -> x

genRandomIV :: (CRT.MonadRandom m) => m (IV AES256)
genRandomIV = do
  bytes :: BS.ByteString <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  case makeIV bytes of
    Just iv -> pure iv
    Nothing -> error "unexpected error: unable to generate random IV"

encrypt, decrypt :: IV AES256 -> BS.ByteString -> BS.ByteString
encrypt = ctrCombine cryptoCipher
decrypt = encrypt

encryptRandom :: (CRT.MonadRandom m) => BS.ByteString -> m (IV AES256, BS.ByteString)
encryptRandom msg = do
  iv <- genRandomIV
  pure (iv, encrypt iv msg)

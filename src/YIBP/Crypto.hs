module YIBP.Crypto
  ( encrypt
  , decrypt
  , encryptRandom
  , Encrypted
  , encryptedToByteString
  , byteStringToEncrypted
  , decrypt'
  , decryptEncryptedRaw
  ) where

import YIBP.Crypto.TH

import Data.ByteArray qualified as BA
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

unsafeMakeIV :: BS.ByteString -> IV AES256
unsafeMakeIV =
  makeIV >>= \case
    Just iv -> pure iv
    Nothing -> error "unexpected error: unable to makeIV"

genRandomIV :: (CRT.MonadRandom m) => m (IV AES256)
genRandomIV = do
  bytes :: BS.ByteString <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  pure $ unsafeMakeIV bytes

encrypt, decrypt :: IV AES256 -> BS.ByteString -> BS.ByteString
encrypt = ctrCombine cryptoCipher
decrypt = encrypt

encryptRandom :: (CRT.MonadRandom m) => BS.ByteString -> m Encrypted
encryptRandom msg = do
  iv <- genRandomIV
  pure $ Encrypted iv (encrypt iv msg)

decrypt' :: Encrypted -> BS.ByteString
decrypt' (Encrypted iv bs) = decrypt iv bs

decryptEncryptedRaw :: BS.ByteString -> BS.ByteString
decryptEncryptedRaw = decrypt' . byteStringToEncrypted

data Encrypted = Encrypted !(IV AES256) !BS.ByteString

encryptedToByteString :: Encrypted -> BS.ByteString
encryptedToByteString (Encrypted iv bs) = BS.append (BA.convert iv) bs

byteStringToEncrypted :: BS.ByteString -> Encrypted
byteStringToEncrypted bs = Encrypted (unsafeMakeIV iv) payload
  where
    (iv, payload) = BS.splitAt (blockSize (undefined :: AES256)) bs

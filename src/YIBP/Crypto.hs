module YIBP.Crypto
  ( EncryptionStatus (..)
  , CryptoText
  , encrypt
  , decrypt
  , encryptRandom
  , EncryptedValue
  , encryptedToByteString
  , byteStringToEncrypted
  , decrypt'
  , decryptEncryptedRaw
  , encryptCryptoTextRandom
  , decryptCryptoText
  ) where

import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, makeIV)
import Crypto.Error
import Crypto.Random.Types qualified as CRT

import YIBP.Config

data EncryptionStatus = Encrypted | Decrypted

type family CryptoText (s :: EncryptionStatus) where
  CryptoText Encrypted = BS.ByteString
  CryptoText Decrypted = T.Text

cryptoCipher :: WithConfig => AES256
cryptoCipher = case cipherInit getConfig.secretKey of
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

encrypt, decrypt :: WithConfig => IV AES256 -> BS.ByteString -> BS.ByteString
encrypt = ctrCombine cryptoCipher
decrypt = encrypt

encryptRandom :: (WithConfig, CRT.MonadRandom m) => BS.ByteString -> m EncryptedValue
encryptRandom msg = do
  iv <- genRandomIV
  pure $ EncryptedValue iv (encrypt iv msg)

encryptCryptoTextRandom
  :: (WithConfig, CRT.MonadRandom m)
  => CryptoText 'Decrypted
  -> m (CryptoText 'Encrypted)
encryptCryptoTextRandom t = encryptedToByteString <$> encryptRandom (T.encodeUtf8 t)

decryptCryptoText
  :: WithConfig
  => CryptoText 'Encrypted
  -> CryptoText 'Decrypted
decryptCryptoText = T.decodeUtf8 . decryptEncryptedRaw

decrypt' :: WithConfig => EncryptedValue -> BS.ByteString
decrypt' (EncryptedValue iv bs) = decrypt iv bs

decryptEncryptedRaw :: WithConfig => BS.ByteString -> BS.ByteString
decryptEncryptedRaw = decrypt' . byteStringToEncrypted

data EncryptedValue = EncryptedValue !(IV AES256) !BS.ByteString

encryptedToByteString :: EncryptedValue -> BS.ByteString
encryptedToByteString (EncryptedValue iv bs) = BS.append (BA.convert iv) bs

byteStringToEncrypted :: BS.ByteString -> EncryptedValue
byteStringToEncrypted bs = EncryptedValue (unsafeMakeIV iv) payload
  where
    (iv, payload) = BS.splitAt (blockSize (undefined :: AES256)) bs

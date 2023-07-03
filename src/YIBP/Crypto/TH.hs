module YIBP.Crypto.TH (cryptoKeyTH) where

import Data.ByteString qualified as BS
import Data.FileEmbed

import Language.Haskell.TH.Syntax

cryptoKeyTH :: FilePath -> Q Exp
cryptoKeyTH fp = do
  qAddDependentFile fp
  content <- runIO $ BS.readFile fp
  if BS.length content == 32
    then bsToExp content
    else fail "The length of the key is not 32 bytes"

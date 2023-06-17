{-# LANGUAGE FieldSelectors #-}
module YIBP.App where

import Control.Monad.Reader (ReaderT)

import Crypto.JOSE (JWK)
import Hasql.Connection (Connection)

type AppT = ReaderT Env

data Env = Env
  { dbConnection :: Connection
  , jwk :: JWK
  }

class Has field env where
  obtain :: env -> field

instance Has Connection Env where obtain = dbConnection

instance Has JWK Env where obtain = jwk
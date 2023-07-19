{-# LANGUAGE FieldSelectors #-}

module YIBP.App where

import Control.Monad.Reader (ReaderT)

import Crypto.JOSE (JWK)
import Hasql.Connection (Connection)

import YIBP.Config

type AppM = IO

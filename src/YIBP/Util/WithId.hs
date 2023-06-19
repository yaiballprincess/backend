{-# LANGUAGE OverloadedStrings #-}

module YIBP.Util.WithId (WithId (..)) where

import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.KeyMap
import Data.Vector qualified as V

data WithId a = WithId !Int !a
  deriving (Show, Eq)

instance (ToJSON a) => ToJSON (WithId a) where
  toJSON (WithId _id val) = Array $ V.fromList [toJSON _id, toJSON val]

instance (FromJSON a) => FromJSON (WithId a) where
  parseJSON = withArray "WithId" $ \v -> do
    case V.toList v of
      [_id, val] -> WithId <$> parseJSON _id <*> parseJSON val
      _ -> fail "expected pair [id, value]"
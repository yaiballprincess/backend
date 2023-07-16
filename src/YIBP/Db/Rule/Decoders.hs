module YIBP.Db.Rule.Decoders where

import Data.Aeson
import Data.Bifunctor
import Data.Text qualified as T

import Hasql.Decoders
import YIBP.Db.Id.Decoders

import YIBP.Db.Rule.Types

ruleRow :: Row RawRule
ruleRow =
  RawRule
    <$> idRow
    <*> column (nonNullable (jsonbBytes decodeJson))
    <*> column (nonNullable bool)
    <*> column (nonNullable bool)
  where
    decodeJson = first T.pack . eitherDecodeStrict

module YIBP.Db.PollTemplate.Decoders where

import Data.Vector qualified as V
import Hasql.Decoders
import YIBP.Core.PollTemplate
import YIBP.Db.Id.Decoders

pollTemplateFullRow :: Row PollTemplateFull
pollTemplateFullRow =
  PollTemplateFull
    <$> idRow
    <*> column (nonNullable bool)
    <*> column (nonNullable bool)
    <*> column (nullable timestamptz)
    <*> column (nonNullable text)
    <*> optionsRow
  where
    optionsRow :: Row (V.Vector PollTemplateOption)
    optionsRow = column (nonNullable (vectorArray (nonNullable (PollTemplateOption <$> text))))

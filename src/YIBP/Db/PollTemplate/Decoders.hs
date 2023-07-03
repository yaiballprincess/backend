module YIBP.Db.PollTemplate.Decoders where

import Data.Vector qualified as V
import Hasql.Decoders
import YIBP.Core.Id
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
    idsVectorRow :: Row (V.Vector (Id PollTemplateOption))
    idsVectorRow = column (nonNullable (vectorArray (nonNullable idValue)))

    textVectorRow :: Row (V.Vector PollTemplateOption)
    textVectorRow = column (nonNullable (vectorArray (nonNullable (PollTemplateOption <$> text))))

    optionsRow :: Row (V.Vector (IdObject PollTemplateOption))
    optionsRow =
      fmap (\(i, t) -> V.map (\(i', t') -> IdObject i' t') $ V.zip i t) $
        (,) <$> idsVectorRow <*> textVectorRow

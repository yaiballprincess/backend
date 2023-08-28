module YIBP.Db.PollTemplate.Encoders where

import Hasql.Encoders

import Data.Functor.Contravariant
import YIBP.Db.Id.Encoders
import YIBP.Db.PollTemplate.Types

insertPollTemplateParams :: Params InsertPollTemplateParams
insertPollTemplateParams =
  ((.isMultiple) >$< param (nonNullable bool))
    <> ((.isAnonymous) >$< param (nonNullable bool))
    <> ((.endsAt) >$< param (nullable timestamptz))
    <> ((.question) >$< param (nonNullable text))
    <> ((.options) >$< param (nonNullable (foldableArray (nonNullable text))))

updatePollTemplateParams :: Params UpdatePollTemplateParams
updatePollTemplateParams =
  ((.id) >$< idParams)
    <> ((.isMultiple) >$< param (nonNullable bool))
    <> ((.isAnonymous) >$< param (nonNullable bool))
    <> ((.endsAt) >$< param (nullable timestamptz))
    <> ((.question) >$< param (nonNullable text))
    <> ((.options) >$< param (nonNullable (foldableArray (nonNullable text))))

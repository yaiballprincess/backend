module YIBP.Db.PollTemplate.Encoders where

import Hasql.Encoders

import Data.Functor.Contravariant
import YIBP.Db.Id.Encoders
import YIBP.Db.PollTemplate.Types

insertPollTemplateParams :: Params InsertPollTemplateParams
insertPollTemplateParams =
  ((\u -> u.isMultiple) >$< param (nonNullable bool))
    <> ((\u -> u.isAnonymous) >$< param (nonNullable bool))
    <> ((\u -> u.endsAt) >$< param (nullable timestamptz))
    <> ((\u -> u.question) >$< param (nonNullable text))

updatePollTemplateParams :: Params UpdatePollTemplateParams
updatePollTemplateParams =
  ((\u -> u.id) >$< idParams)
    <> ((\u -> u.isMultiple) >$< param (nonNullable bool))
    <> ((\u -> u.isAnonymous) >$< param (nonNullable bool))
    <> ((\u -> u.endsAt) >$< param (nullable timestamptz))
    <> ((\u -> u.question) >$< param (nonNullable text))

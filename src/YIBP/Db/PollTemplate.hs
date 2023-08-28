{-# LANGUAGE OverloadedStrings #-}

module YIBP.Db.PollTemplate where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement
import YIBP.Db
import YIBP.Db.Id.Decoders
import YIBP.Db.Id.Encoders
import YIBP.Db.Id.Encoders qualified as Encoders
import YIBP.Db.PollTemplate.Decoders
import YIBP.Db.PollTemplate.Encoders
import YIBP.Db.PollTemplate.Types

import Data.Time
import Data.Vector qualified as V

import GHC.Generics
import GHC.Stack

import Contravariant.Extras

import YIBP.Core.Id
import YIBP.Core.PollTemplate qualified as Core

getAllPollTemplates :: (WithDb, HasCallStack) => IO (V.Vector Core.PollTemplateFull)
getAllPollTemplates = withConn $ Session.run (Session.statement () stmt)
  where
    stmt =
      Statement
        "select id, is_multiple, is_anonymous, duration, question, options \
        \ from \"poll_template\""
        Encoders.noParams
        (Decoders.rowVector pollTemplateFullRow)
        True

insertPollTemplate
  :: (WithDb, HasCallStack)
  => InsertPollTemplateParams
  -> IO (Id Core.PollTemplate)
insertPollTemplate params = withConn $ Session.run (Session.statement params stmt)
  where
    stmt =
      Statement
        "insert into \"poll_template\" \
        \ (is_multiple, is_anonymous, duration, question, options) \
        \ values ($1, $2, $3, $4, $5) \
        \ returning id"
        insertPollTemplateParams
        (Decoders.singleRow idRow)
        True

updatePollTemplate
  :: (WithDb, HasCallStack)
  => UpdatePollTemplateParams
  -> IO Bool
updatePollTemplate params =
  withConn $ Session.run ((== 1) <$> Session.statement params stmt)
  where
    stmt =
      Statement
        "update \"poll_template\" set \
        \ is_multiple = $2, is_anonymous = $3, \
        \ duration = $4, question = $5, options = $6 \
        \ where id = $1"
        updatePollTemplateParams
        Decoders.rowsAffected
        True

deletePollTemplate :: (WithDb, HasCallStack) => Id Core.PollTemplate -> IO Bool
deletePollTemplate ptId =
  withConn $ Session.run ((== 1) <$> Session.statement ptId stmt)
  where
    stmt =
      Statement
        "delete from \"poll_template\" where id = $1"
        idParams
        Decoders.rowsAffected
        True

getPollTemplatesByIds
  :: (WithDb, HasCallStack)
  => V.Vector (Id Core.PollTemplate)
  -> IO (V.Vector Core.PollTemplateFull)
getPollTemplatesByIds ids =
  withConn $ Session.run (Session.statement ids stmt)
  where
    stmt =
      Statement
        "select id, is_multiple, is_anonymous, duration, question, options \
        \ from \"poll_template\" as pt \
        \ where id = ANY($1)"
        (Encoders.param $ Encoders.nonNullable $ Encoders.foldableArray $ Encoders.nonNullable Encoders.idValue)
        (Decoders.rowVector pollTemplateFullRow)
        True

getPollTemplateById :: (WithDb, HasCallStack) => Id Core.PollTemplate -> IO (Maybe Core.PollTemplateFull)
getPollTemplateById pId = do
  vec <- getPollTemplatesByIds (V.singleton pId)
  pure $ case V.uncons vec of
    Just (t, _) -> Just t
    Nothing -> Nothing

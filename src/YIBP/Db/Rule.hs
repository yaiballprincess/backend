{-# LANGUAGE OverloadedStrings #-}

module YIBP.Db.Rule where

import Hasql.Session qualified as Session
import Hasql.Statement

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E

-- import Data.Text qualified as T
import Data.Vector qualified as V

--

import Contravariant.Extras
import YIBP.Core.Id
import YIBP.Core.Rule
import YIBP.Db
import YIBP.Db.Id.Decoders
import YIBP.Db.Id.Encoders
import YIBP.Db.Id.Encoders qualified as E
import YIBP.Db.Rule.Decoders
import YIBP.Db.Rule.Encoders
import YIBP.Db.Rule.Types

getRuleById :: (WithDb) => Id Rule -> IO (Maybe RawRule)
getRuleById ruleId = withConn $ Session.run (Session.statement ruleId stmt)
  where
    stmt =
      Statement
        "select id, metadata, can_trigger, is_active from \"rule\" where id = $1"
        E.idParams
        (D.rowMaybe ruleRow)
        True

getAllRules :: (WithDb) => IO (V.Vector RawRule)
getAllRules = withConn $ Session.run (Session.statement () stmt)
  where
    stmt =
      Statement
        "select id, metadata, can_trigger, is_active from \"rule\""
        E.noParams
        (D.rowVector ruleRow)
        True

getAllRulesCanTrigger :: (WithDb) => IO (V.Vector RawRule)
getAllRulesCanTrigger = withConn $ Session.run (Session.statement () stmt)
  where
    stmt =
      Statement
        "select id, metadata, can_trigger, is_active from \"rule\" \
        \ where can_trigger = true"
        E.noParams
        (D.rowVector ruleRow)
        True

insertRule :: (WithDb) => Rule -> IO (Id Rule)
insertRule r = withConn $ Session.run (Session.statement r stmt)
  where
    stmt =
      Statement
        "insert into \"rule\" (metadata, can_trigger, is_active) \
        \ values ($1, true, $2) \
        \ returning id"
        ruleParams
        (D.singleRow idRow)
        True

updateRule :: (WithDb) => Id Rule -> Rule -> IO Bool
updateRule rId rule = withConn $ Session.run ((== 1) <$> Session.statement (rId, rule) stmt)
  where
    stmt =
      Statement
        "update \"rule\" set metadata = $2, is_active = $3 \
        \ where id = $1"
        (contrazip2 idParams ruleParams)
        D.rowsAffected
        True

deleteRule :: (WithDb) => Id Rule -> IO Bool
deleteRule rId = withConn $ Session.run ((== 1) <$> Session.statement rId stmt)
  where
    stmt =
      Statement
        "delete from \"rule\" where id = $1"
        idParams
        D.rowsAffected
        True

setCanTriggerFalseBatch :: (WithDb) => V.Vector (Id Rule) -> IO ()
setCanTriggerFalseBatch vec = withConn $ Session.run (Session.statement vec stmt)
  where
    stmt =
      Statement
        "update \"rule\" set can_trigger = false where id = ANY($1)"
        (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.idValue))))
        D.noResult
        True

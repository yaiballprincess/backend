{-# LANGUAGE OverloadedStrings #-}
module YIBP.Db.Rule where

import Hasql.Session qualified as Session
import Hasql.Statement

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E

-- import Data.Text qualified as T
import Data.Vector qualified as V
-- 
import YIBP.Core.Rule
import YIBP.Db
import YIBP.Db.Rule.Types
import YIBP.Db.Rule.Encoders
import YIBP.Db.Rule.Decoders

getAllRules :: WithDb => IO (V.Vector RawRule)
getAllRules = withConn $ Session.run (Session.statement () stmt)
  where
    stmt =
      Statement
        "select id, metadata, can_trigger, is_active from \"rule\""
        E.noParams
        (D.rowVector ruleRow)
        True

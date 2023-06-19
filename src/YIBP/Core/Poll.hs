{-# LANGUAGE DeriveAnyClass #-}

module YIBP.Core.Poll where

import Data.Aeson
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V
import GHC.Generics

import Optics

type PollOption = T.Text

data PollTemplate = PollTemplate
  { isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  , options :: !(V.Vector PollOption)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makeFieldsNoPrefix ''PollTemplate
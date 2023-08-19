{-# LANGUAGE OverloadedStrings #-}

module YIBP.Core.User (User (..), CreateUser (..), LoginUser (..), CreateUserParam, LoginUserParam, validateCreateUser, validateLoginUser) where

import Data.Aeson
import Data.Bifunctor
import Data.Char
import Data.Functor
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics
import YIBP.Validate

count :: (Char -> Bool) -> T.Text -> Int
count predicate = T.foldl' go 0
  where
    go :: Int -> Char -> Int
    go acc ch = if predicate ch then acc + 1 else acc

data User = User
  { username :: !T.Text
  , isAdmin :: !Bool
  }

newtype UsernameParam = UsernameParam T.Text
  deriving newtype (FromJSON, ToJSON)

data UsernameValidationError
  = UsernameIsTooShort
  | UsernameContainsIllegalChars

instance Show UsernameValidationError where
  show UsernameIsTooShort = "username should be at least 5 characters long"
  show UsernameContainsIllegalChars = "username should contain only digits or letters"

validateUsername :: UsernameParam -> ValidatedVector UsernameValidationError T.Text
validateUsername (UsernameParam rawUsername) =
  ( validatedVectorSimple (T.length rawUsername >= 5) UsernameIsTooShort
      *> validatedVectorSimple (T.all (\ch -> isLetter ch || isDigit ch) rawUsername) UsernameContainsIllegalChars
  )
    $> rawUsername

newtype PasswordParam = PasswordParam T.Text
  deriving newtype (FromJSON, ToJSON)

data PasswordValidationError
  = PasswordIsTooShort
  | PasswordContainsNotEnoughDigits
  | PasswordContainsNotEnoughLetters

instance Show PasswordValidationError where
  show PasswordIsTooShort = "password should be at least 8 characters long"
  show PasswordContainsNotEnoughDigits = "password should contain at least 2 digits"
  show PasswordContainsNotEnoughLetters = "password should contain at least 4 letters"

validatePassword :: PasswordParam -> ValidatedVector PasswordValidationError T.Text
validatePassword (PasswordParam rawPassword)
  | T.length rawPassword >= 16 = pure rawPassword
  | otherwise =
      ( validatedVectorSimple (T.length rawPassword >= 8) PasswordIsTooShort
          *> validatedVectorSimple (count isDigit rawPassword >= 2) PasswordContainsNotEnoughDigits
          *> validatedVectorSimple (count isLetter rawPassword >= 4) PasswordContainsNotEnoughLetters
      )
        $> rawPassword

data CreateUserParam = CreateUserParam
  { username :: !UsernameParam
  , password :: !PasswordParam
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateUser = CreateUser
  { username :: !T.Text
  , password :: !T.Text
  }

validateCreateUser :: CreateUserParam -> Either (ValidationError ('Just (V.Vector T.Text))) CreateUser
validateCreateUser cu =
  castValidated "CreateUserParam is invalid" $
    CreateUser
      <$> first (V.map (T.pack . show)) (validateUsername cu.username)
      <*> first (V.map (T.pack . show)) (validatePassword cu.password)

data LoginUserParam = LoginUserParam
  { username :: !UsernameParam
  , password :: !PasswordParam
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

data LoginUser = LoginUser
  { username :: !T.Text
  , password :: !T.Text
  }

validateLoginUser :: LoginUserParam -> Either (ValidationError ('Just (V.Vector T.Text))) LoginUser
validateLoginUser lu =
  castValidated "LoginUserParam is invalid" $
    LoginUser
      <$> first (V.map (T.pack . show)) (validateUsername lu.username)
      <*> first (V.map (T.pack . show)) (validatePassword lu.password)

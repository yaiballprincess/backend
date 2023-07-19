module YIBP.Validate
  ( ValidationError (..)
  , validateOrThrowServerError
  , throwValidationError
  , removeDetails
  , Validated
  , ValidatedList
  , ValidatedVector
  , validatedSimple
  , validatedListSimple
  , validatedVectorSimple
  , castValidated
  ) where

import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor
import Data.Kind
import Data.Text qualified as T
import Data.Typeable
import Data.Vector qualified as V
import Servant
import YIBP.Error

type ValidationError :: Maybe Type -> Type
data family ValidationError details
data instance ValidationError ('Just details) = ValidationErrorWithDetails !T.Text !details
newtype instance ValidationError 'Nothing = ValidationError T.Text

deriving instance Typeable (ValidationError details)

removeDetails :: ValidationError ('Just details) -> ValidationError 'Nothing
removeDetails (ValidationErrorWithDetails t _) = ValidationError t

validateOrThrowServerError
  :: forall m result details
   . ( MonadError ServerError m
     , ValidationErrorToHttpError details
     , ToJSON (HttpError (ValidationError details) details)
     )
  => Either (ValidationError details) result
  -> m result
validateOrThrowServerError = \case
  Right x -> pure x
  Left err -> throwValidationError err

throwValidationError :: forall m a details. (MonadError ServerError m, ValidationErrorToHttpError details, ToJSON (HttpError (ValidationError details) details)) => ValidationError details -> m a
throwValidationError err = throwError $ transformServantError (validationErrorToHttpError err) err400

class ValidationErrorToHttpError details where
  validationErrorToHttpError :: ValidationError details -> HttpError (ValidationError details) details

instance ValidationErrorToHttpError ('Just details) where
  validationErrorToHttpError (ValidationErrorWithDetails t d) = HttpErrorWithDetails t d

instance ValidationErrorToHttpError 'Nothing where
  validationErrorToHttpError (ValidationError t) = HttpError t

newtype Validated e a = Validated (Either e a)
  deriving newtype (Functor, Bifunctor)

type ValidatedList e a = Validated [e] a
type ValidatedVector e a = Validated (V.Vector e) a

instance (Semigroup e) => Applicative (Validated e) where
  pure x = Validated (Right x)
  vf <*> va = case vf of
    Validated (Left e) -> case va of
      Validated (Left ea) -> Validated (Left (e <> ea))
      Validated (Right _) -> Validated (Left e)
    Validated (Right f) -> case va of
      Validated (Left ea) -> Validated (Left ea)
      Validated (Right x) -> Validated (Right (f x))

-- | The 'validatedSimple' function returns error 'e' if 'False'
validatedSimple :: Bool -> e -> Validated e ()
validatedSimple True _ = Validated $ Right ()
validatedSimple False e = Validated $ Left e

validatedListSimple :: Bool -> e -> ValidatedList e ()
validatedListSimple True _ = Validated $ Right ()
validatedListSimple False e = Validated $ Left [e]

validatedVectorSimple :: Bool -> e -> ValidatedVector e ()
validatedVectorSimple True _ = Validated $ Right ()
validatedVectorSimple False e = Validated $ Left (V.singleton e)

castValidated :: T.Text -> Validated e a -> Either (ValidationError ('Just e)) a
castValidated t (Validated (Left e)) = Left (ValidationErrorWithDetails t e)
castValidated _ (Validated (Right p)) = Right p

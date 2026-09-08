{-# LANGUAGE FlexibleContexts #-}
module Evaluator.Errors (runtimeErrorValue, valueRuntimeError, evalRecoveryWith) where

import Control.Monad.Except (MonadError, catchError, throwError, liftEither)
import Evaluator.Types
import Evaluator.Functions (applyCallableWith)
import Syntax

-- Only explicitly recoverable language failures cross an attempt boundary.
-- Exit, cancellation, invariant failures and host exceptions are not converted.
runtimeErrorValue :: RuntimeError -> Maybe Value
runtimeErrorValue (RuntimeAt _ failure) = runtimeErrorValue failure
runtimeErrorValue (RuntimeContext _ _ failure) = runtimeErrorValue failure
runtimeErrorValue DivByZero = Just $ VData "DivisionByZero" []
runtimeErrorValue IntegerOverflow = Just $ VData "ArithmeticOverflow" []
runtimeErrorValue (EmptyListError operation) = Just $ VData "EmptyList" [VStr operation]
runtimeErrorValue EndOfInputError = Just $ VData "EndOfInput" []
runtimeErrorValue (IOFailure category operation path detail) = Just $
  VData "IOError" [VData (show category) [], VStr operation,
    maybe VNothing (VJust . VStr) path, VStr detail]
runtimeErrorValue (UserFailure message) = Just $ VData "UserError" [VStr message]
runtimeErrorValue _ = Nothing

valueRuntimeError :: Value -> Either RuntimeError RuntimeError
valueRuntimeError (VData "DivisionByZero" []) = Right DivByZero
valueRuntimeError (VData "ArithmeticOverflow" []) = Right IntegerOverflow
valueRuntimeError (VData "EmptyList" [VStr operation]) = Right $ EmptyListError operation
valueRuntimeError (VData "EndOfInput" []) = Right EndOfInputError
valueRuntimeError (VData "UserError" [VStr message]) = Right $ UserFailure message
valueRuntimeError (VData "IOError" [VData category [], VStr operation, path, VStr detail]) = do
  kind <- case [k | k <- [minBound .. maxBound], show k == category] of
    [k] -> Right k
    _ -> Left invalidError
  optionalPath <- case path of
    VNothing -> Right Nothing
    VJust (VStr text) -> Right (Just text)
    _ -> Left invalidError
  Right $ IOFailure kind operation optionalPath detail
valueRuntimeError _ = Left invalidError

invalidError :: RuntimeError
invalidError = TypeError "raise expects a valid Error value"

evalRecoveryWith :: MonadError RuntimeError m => (Value -> m Value) -> Eval m -> Eval m
evalRecoveryWith resolve evaluate env (Attempt expression) = do
  action <- evaluate env expression
  catchError (VRight <$> applyCallableWith resolve evaluate action VUnit) $ \failure ->
    maybe (throwError failure) (pure . VLeft) (runtimeErrorValue failure)
evalRecoveryWith _ evaluate env (Raise expression) = do
  value <- evaluate env expression
  failure <- liftEither $ valueRuntimeError value
  throwError failure
evalRecoveryWith _ _ _ _ = error "evalRecoveryWith called on non-recovery expression"

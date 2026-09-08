module Diagnostics
  ( renderTypeError
  , renderType
  , renderRuntimeError
  , renderTypeWarning
  , renderSourceMessage
  ) where

import qualified Data.Text as Text
import Syntax (SourceSpan(..))
import qualified TypeChecker.Types as T
import TypeChecker.Pretty (renderType, renderPredicate)
import qualified Evaluator.Types as E

renderTypeError :: T.TypeError -> String
renderTypeError (T.TypeAt location failure) = renderSourceMessage location (renderTypeError failure)
renderTypeError (T.TypeImport location name failure) = renderTypeError failure ++ "\n" ++
  renderSourceMessage location ("while importing module " ++ name)
renderTypeError failure = "Type error: " ++ case failure of
  T.TypeMismatch a b -> mismatch a b
  T.UnificationError a b -> mismatch a b
  T.ExpectedInt ty -> "Expected Int, but found " ++ renderType ty ++ "."
  T.ExpectedBool ty -> "Expected Bool, but found " ++ renderType ty ++ "."
  T.ExpectedFunction ty -> "Expected a function, but found " ++ renderType ty ++ "."
  T.UnboundVariable name -> "Unknown name '" ++ name ++ "'."
  T.InfiniteType name ty -> "An infinite type would be required: " ++ name ++ " occurs in " ++ renderType ty ++ "."
  T.RecordFieldMismatch name -> "Missing record field '" ++ name ++ "'."
  T.InvalidWildcard message -> message
  T.GeneralTypeError message -> message
  T.DuplicatePatternBinding name -> "Pattern binds '" ++ name ++ "' more than once."
  T.DuplicateRecordField name -> "Duplicate record field '" ++ name ++ "'."
  T.InvalidDataDeclaration message -> message
  T.ConstructorPatternArity name expected actual ->
    "Pattern '" ++ name ++ "' needs " ++ show expected ++ " fields, but has " ++ show actual ++ "."
  T.KindMismatch _ _ -> "A record row and a value type cannot be used interchangeably."
  T.ConflictingVariableKind name -> "'" ++ name ++ "' is used as both a record row and a value type."
  T.UnsatisfiedConstraint predicate -> case T.unlocatedPredicate predicate of
    T.Equality ty -> "Equality is not supported for " ++ renderType ty ++ "; all stored payloads must be comparable."
    T.Appendable ty -> "Cannot concatenate " ++ renderType ty ++ "; ++ requires strings or lists."
    _ -> "Unsupported constraint " ++ renderPredicate predicate ++ "."
  T.AmbiguousConstraint predicate -> "Cannot resolve " ++ renderPredicate predicate ++ "; supply a concrete type."
  T.MissingConstraint predicate -> "The annotation must include " ++ renderPredicate predicate ++ "."
  T.NonExhaustivePatterns witness -> "Incomplete case; add a branch covering " ++ witness ++ "."
  where mismatch a b = "Cannot match " ++ renderType a ++ " with " ++ renderType b ++ "."

renderRuntimeError :: E.RuntimeError -> String
renderRuntimeError failure =
  let (contexts,core) = collect failure
      message = case core of
        E.RuntimeAt location inner -> renderSourceMessage location (runtimeMessage inner)
        other -> runtimeMessage other
      shown = take 12 contexts
      more = ["  ... further calls omitted" | length contexts > length shown]
  in unlinesWithoutFinal (message : map renderContext shown ++ more)
  where
    collect (E.RuntimeContext context location inner) =
      let (rest,core) = collect inner in ((context,location):rest,core)
    collect value = ([],value)
    renderContext (context,location) = "  " ++ context ++ " at " ++ sourcePosition location

runtimeMessage :: E.RuntimeError -> String
runtimeMessage (E.RuntimeAt _ inner) = runtimeMessage inner
runtimeMessage (E.RuntimeContext _ _ inner) = runtimeMessage inner
runtimeMessage failure = "Runtime error: " ++ case failure of
  E.DivByZero -> "Division by zero."
  E.IntegerOverflow -> "Integer overflow; Int uses the signed 32-bit range."
  E.UninitializedRecursion name -> "Recursive binding '" ++ name ++ "' was read before initialization."
  E.TypeError message -> message
  E.UnboundVariable name -> "Unknown name '" ++ name ++ "'."
  E.RecordFieldNotFound name -> "Missing record field '" ++ name ++ "'."
  E.EmptyListError operation -> operation ++ " requires a nonempty list."
  E.EndOfInputError -> "End of input; use readLine () to handle normal EOF."
  E.IOFailure category operation path detail ->
    operation ++ maybe "" (\value -> " '" ++ value ++ "'") path ++ ": " ++ categoryMessage category ++
      (if null detail then "." else ".\nHost detail: " ++ detail)
  E.UserFailure message -> message
  E.ExitRequested code -> "Process exited with status " ++ show code ++ "."

categoryMessage :: E.IOErrorKind -> String
categoryMessage E.NotFound = "not found"
categoryMessage E.PermissionDenied = "permission denied"
categoryMessage E.AlreadyExists = "already exists"
categoryMessage E.InvalidPath = "invalid path"
categoryMessage E.InvalidEncoding = "invalid UTF-8"
categoryMessage E.ResourceBusy = "resource busy"
categoryMessage E.OtherIO = "I/O operation failed"

renderTypeWarning :: T.TypeWarning -> String
renderTypeWarning (T.UnreachableAlternative index) = "Warning: case alternative " ++ show index ++ " is unreachable"
renderTypeWarning (T.InModule path warning) = "In module " ++ path ++ ":\n" ++ renderTypeWarning warning
renderTypeWarning (T.WarningAt location warning) = renderSourceMessage location (renderTypeWarning warning)

sourcePosition :: SourceSpan -> String
sourcePosition location = spanFile location ++ ":" ++ show (spanLine location) ++ ":" ++ show (spanColumn location)

renderSourceMessage :: SourceSpan -> String -> String
renderSourceMessage location message =
  let number = show (spanLine location)
      excerpt = expandTabs 1 (Text.unpack (spanExcerpt location))
  in sourcePosition location ++ ": " ++ message ++ "\n" ++
     number ++ " | " ++ excerpt ++ "\n" ++
     replicate (length number) ' ' ++ " | " ++ replicate (max 0 (spanColumn location-1)) ' ' ++ "^"
  where
    expandTabs _ [] = []
    expandTabs column ('\t':rest) = let width = 8 - ((column-1) `mod` 8)
      in replicate width ' ' ++ expandTabs (column+width) rest
    expandTabs column (c:rest) = c : expandTabs (column+1) rest

unlinesWithoutFinal :: [String] -> String
unlinesWithoutFinal [] = ""
unlinesWithoutFinal [line] = line
unlinesWithoutFinal (line:rest) = line ++ "\n" ++ unlinesWithoutFinal rest

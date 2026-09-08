{-# LANGUAGE LambdaCase #-}
module TypeChecker.Pretty (renderType, renderPredicate) where

import TypeChecker.Types
import qualified Data.Map as Map
import qualified Data.List as List

renderType :: Type -> String
renderType ty =
  case ty of
    TQualified predicates value ->
      let names = map renderPredicate predicates
          context = case names of
            [name] -> name
            _ -> "(" ++ List.intercalate ", " names ++ ")"
      in context ++ " => " ++ renderType value
    TFun left right -> renderTypeAtom left ++ " -> " ++ renderType right
    TCustom name args -> unwords (name : map renderTypeAtom args)
    TMaybe value -> "Maybe " ++ renderTypeAtom value
    TEither left right -> "Either " ++ renderTypeAtom left ++ " " ++ renderTypeAtom right
    _ -> renderTypeAtom ty

renderPredicate :: Predicate -> String
renderPredicate (PredicateAt _ predicate) = renderPredicate predicate
renderPredicate (Equality ty) = "Eq " ++ renderTypeAtom ty
renderPredicate (Appendable ty) = "Append " ++ renderTypeAtom ty

renderTypeAtom :: Type -> String
renderTypeAtom = \case
  ty@TQualified {} -> "(" ++ renderType ty ++ ")"
  TInt -> "Int"
  TBool -> "Bool"
  TString -> "String"
  TUnit -> "Unit"
  TVar name -> name
  TCustom name [] -> name
  ty@(TCustom _ (_:_)) -> "(" ++ renderType ty ++ ")"
  ty@TMaybe {} -> "(" ++ renderType ty ++ ")"
  ty@TEither {} -> "(" ++ renderType ty ++ ")"
  TList ty -> "[" ++ renderType ty ++ "]"
  TRecord fields ->
    "{" ++ List.intercalate ", " [name ++ ": " ++ renderType fieldType | (name, fieldType) <- Map.toList fields] ++ "}"
  TOpenRecord fields row ->
    "{" ++ List.intercalate ", " [name ++ ": " ++ renderType fieldType | (name, fieldType) <- Map.toList fields] ++ " | " ++ renderType row ++ "}"
  TRowVar name -> name
  TRowEmpty -> "{}"
  TTuple tys ->
    "(" ++ List.intercalate ", " (map renderType tys) ++ ")"
  ty@TFun {} -> "(" ++ renderType ty ++ ")"

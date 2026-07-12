module TopLevelRecursion
  ( collectConsecutiveLetrecs
  , dependencyOrderedLetrecGroups
  ) where

import Syntax
import Data.Graph (SCC(..), stronglyConnComp)
import qualified Data.Set as Set

collectConsecutiveLetrecs :: [TopLevel] -> ([TopLevel], [TopLevel])
collectConsecutiveLetrecs [] = ([], [])
collectConsecutiveLetrecs (TLDef var maybeType expr : rest) =
  case expr of
    LetRec _ _ _ _ ->
      let (moreLetrecs, remaining) = collectConsecutiveLetrecs rest
      in (TLDef var maybeType expr : moreLetrecs, remaining)
    _ -> ([], TLDef var maybeType expr : rest)
collectConsecutiveLetrecs (other : rest) = ([], other : rest)

dependencyOrderedLetrecGroups :: [TopLevel] -> [[TopLevel]]
dependencyOrderedLetrecGroups letrecs =
  map flattenScc $
    stronglyConnComp
      [ (topLevel, name, referencedLetrecNames letrecNames (dependencyExpr topLevel))
      | topLevel@(TLDef name _ _) <- letrecs
      ]
  where
    letrecNames = Set.fromList
      [ name
      | TLDef name _ expr <- letrecs
      , isLetrec expr
      ]

    flattenScc (AcyclicSCC topLevel) = [topLevel]
    flattenScc (CyclicSCC group) = group

    isLetrec (LetRec _ _ _ _) = True
    isLetrec _ = False

    dependencyExpr (TLDef _ _ (LetRec _ _ val _)) = val
    dependencyExpr (TLDef _ _ expr) = expr
    dependencyExpr _ = error "dependencyExpr: expected TLDef"

referencedLetrecNames :: Set.Set String -> Expr -> [String]
referencedLetrecNames candidates = Set.toList . go Set.empty
  where
    go bound expr = case expr of
      IntLit _ -> Set.empty
      BoolLit _ -> Set.empty
      StrLit _ -> Set.empty
      UnitLit -> Set.empty
      Input -> Set.empty
      Var name
        | name `Set.member` bound -> Set.empty
        | name `Set.member` candidates -> Set.singleton name
        | otherwise -> Set.empty
      Add e1 e2 -> go2 bound e1 e2
      Sub e1 e2 -> go2 bound e1 e2
      Mul e1 e2 -> go2 bound e1 e2
      Div e1 e2 -> go2 bound e1 e2
      Concat e1 e2 -> go2 bound e1 e2
      And e1 e2 -> go2 bound e1 e2
      Or e1 e2 -> go2 bound e1 e2
      Not e -> go bound e
      Seq e1 e2 -> go2 bound e1 e2
      Eq e1 e2 -> go2 bound e1 e2
      Lt e1 e2 -> go2 bound e1 e2
      Gt e1 e2 -> go2 bound e1 e2
      If cond thenExpr elseExpr -> go3 bound cond thenExpr elseExpr
      Print e -> go bound e
      Discard e -> go bound e
      Lambda name _ body -> go (Set.insert name bound) body
      App e1 e2 -> go2 bound e1 e2
      Let name _ val body -> go bound val `Set.union` go (Set.insert name bound) body
      LetRec name _ val body ->
        let bound' = Set.insert name bound
        in go bound' val `Set.union` go bound' body
      TypeAnnotation e _ -> go bound e
      ParseInt e -> go bound e
      ToString e -> go bound e
      Show e -> go bound e
      MJust e -> go bound e
      MNothing -> Set.empty
      ELeft e -> go bound e
      ERight e -> go bound e
      Case scrutinee branches ->
        go bound scrutinee `Set.union`
          Set.unions
            [ go (Set.union bound (patternBindings pattern')) branchExpr
            | (pattern', branchExpr) <- branches
            ]
      ListLit exprs -> Set.unions (map (go bound) exprs)
      Cons e1 e2 -> go2 bound e1 e2
      Head e -> go bound e
      Tail e -> go bound e
      Null e -> go bound e
      Fix e -> go bound e
      RecordLit fields -> Set.unions [go bound fieldExpr | (_, fieldExpr) <- fields]
      RecordAccess recordExpr _ -> go bound recordExpr
      TupleLit exprs -> Set.unions (map (go bound) exprs)
      Fst e -> go bound e
      Snd e -> go bound e
      Map e1 e2 -> go2 bound e1 e2
      Filter e1 e2 -> go2 bound e1 e2
      Foldl e1 e2 e3 -> go3 bound e1 e2 e3
      Length e -> go bound e
      Reverse e -> go bound e
      Take e1 e2 -> go2 bound e1 e2
      Drop e1 e2 -> go2 bound e1 e2
      Zip e1 e2 -> go2 bound e1 e2
      Split e1 e2 -> go2 bound e1 e2
      Join e1 e2 -> go2 bound e1 e2
      Trim e -> go bound e
      Replace e1 e2 e3 -> go3 bound e1 e2 e3
      StrLength e -> go bound e
      ReadFile e -> go bound e
      WriteFile e1 e2 -> go2 bound e1 e2
      AppendFile e1 e2 -> go2 bound e1 e2
      FileExists e -> go bound e
      ListDirectory e -> go bound e
      CreateDirectory e -> go bound e
      RemoveDirectory e -> go bound e
      GetCurrentDirectory -> Set.empty
      SetCurrentDirectory e -> go bound e
      System e -> go bound e
      GetEnv e -> go bound e
      SetEnv e1 e2 -> go2 bound e1 e2
      Exit e -> go bound e
      Args -> Set.empty

    go2 bound e1 e2 = go bound e1 `Set.union` go bound e2
    go3 bound e1 e2 e3 = go bound e1 `Set.union` go bound e2 `Set.union` go bound e3

patternBindings :: Pattern -> Set.Set String
patternBindings pattern' = case pattern' of
  PVar name -> Set.singleton name
  PInt _ -> Set.empty
  PBool _ -> Set.empty
  PStr _ -> Set.empty
  PUnit -> Set.empty
  PJust pat -> patternBindings pat
  PNothing -> Set.empty
  PLeft pat -> patternBindings pat
  PRight pat -> patternBindings pat
  PList pats -> Set.unions (map patternBindings pats)
  PCons p1 p2 -> patternBindings p1 `Set.union` patternBindings p2
  PRecord fields -> Set.unions [patternBindings pat | (_, pat) <- fields]
  PTuple pats -> Set.unions (map patternBindings pats)
  PConstructor _ pats -> Set.unions (map patternBindings pats)

-------------------------------------------------------------------------------
-- Pretty printer for the Imp language
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
module Lang.Imp.Pretty where
import Lang.Imp.Ast

todo :: a
todo = error "TODO"

-- | Pretty print an expression
ppExpr :: Expr a -> String
ppExpr (AConst n) = show n
ppExpr (AVar x) = x

ppExpr (APlus l r) =
  "(" ++ ppExpr l ++ " + " ++ ppExpr r ++ ")"

ppExpr (AMinus l r) =
  "(" ++ ppExpr l ++ " - " ++ ppExpr r ++ ")"

ppExpr (AMul l r) =
  "(" ++ ppExpr l ++ " * " ++ ppExpr r ++ ")"

ppExpr (ADiv l r) =
  "(" ++ ppExpr l ++ " / " ++ ppExpr r ++ ")"

ppExpr (AMod l r) =
  "(" ++ ppExpr l ++ " % " ++ ppExpr r ++ ")"

ppExpr (REq l r) =
  "(" ++ ppExpr l ++ " = " ++ ppExpr r ++ ")"

ppExpr (RLt l r) =
  "(" ++ ppExpr l ++ " < " ++ ppExpr r ++ ")"

ppExpr (BNot b) =
  "not " ++ ppExpr b

ppExpr (BAnd l r) =
  "(" ++ ppExpr l ++ " && " ++ ppExpr r ++ ")"

ppExpr (BOr l r) =
  "(" ++ ppExpr l ++ " || " ++ ppExpr r ++ ")"

ppCmd :: Cmd -> String
ppCmd CSkip = "skip"

ppCmd (CAssign x e) =
  x ++ " := " ++ ppExpr e

ppCmd (CIfThEl b t e) =
  "if " ++ ppExpr b ++ " then\n"
  ++ ppCmd t
  ++ "\nelse\n"
  ++ ppCmd e
  ++ "\nendif"

ppCmd (CWhile b c) =
  "while " ++ ppExpr b ++ " do\n"
  ++ ppCmd c
  ++ "\nendwhile"

ppCmd (CSeq cs) =
  unlines (map ppCmd cs)
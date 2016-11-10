module Ai.Planning.GeneralReg where

import Ai.Planning.Types

data Literal
    = LBool !Bool
    | LAtom !Atom
    deriving (Show, Eq)

data Expr
    = ELit !Literal
    | EAnd !Expr !Expr
    | EOr !Expr !Expr
    | EEff !Expr !Expr
    deriving (Show, Eq)

epc :: Literal -> Expr -> Expr
epc lit expr =
    simplify $
    case expr of
      ELit lit'
          | lit == lit' -> ELit (LBool True)
          | otherwise -> ELit (LBool False)
      EAnd e e' -> epc lit e `EOr` epc lit e'
      EEff x e -> epc lit e `EAnd` x
      EOr a b -> EOr a b

simplify :: Expr -> Expr
simplify e =
    case e of
      EOr a b
          | a == b -> simplify a
          | a == ELit (LBool True) || b == ELit (LBool True) -> ELit (LBool True)
          | otherwise -> EOr (simplify a) (simplify b)
      EAnd a b
          | a == b -> simplify a
          | a == ELit (LBool False) || b == ELit (LBool False) -> ELit (LBool False)
          | a == ELit (LBool True) -> simplify b
          | b == ELit (LBool True) -> simplify a
          | otherwise -> EAnd (simplify a) (simplify b)
      _ -> e

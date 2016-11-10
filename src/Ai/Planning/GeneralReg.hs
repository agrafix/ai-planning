{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Ai.Planning.GeneralReg where

import Ai.Planning.Types

import Control.Monad
import Data.List (foldl')
import Data.Monoid
import qualified Data.Set as S

data Literal
    = LBool !Bool
    | LAtom !Atom
    deriving (Show, Eq)

instance PrettyText Literal where
    prettyT l =
        case l of
          LBool True -> "⊤"
          LBool False -> "⊥"
          LAtom x -> prettyT x

data Expr
    = ELit !Literal
    | EAnd !Expr !Expr
    | EOr !Expr !Expr
    | EEff !Expr !Expr
    | ENot !Expr
    deriving (Show, Eq)

instance PrettyText Expr where
    prettyT e =
        case e of
          ELit l -> prettyT l
          EAnd a b -> "(" <> prettyT a <> " ∧ " <> prettyT b <> ")"
          EOr a b -> "(" <> prettyT a <> " ∨ " <> prettyT b <> ")"
          EEff a b -> "(" <> prettyT a <> " ▶ " <> prettyT b <> ")"
          ENot a -> "¬" <> prettyT a

(.&&) :: Expr -> Expr -> Expr
(.&&) = EAnd

infixr 3 .&&

(.||) :: Expr -> Expr -> Expr
(.||) = EOr

infixr 2 .||

(.>) :: Expr -> Expr -> Expr
(.>) = EEff

type Operator = (Expr, Expr)

replace :: Expr -> Expr -> Expr -> Expr
replace orig search repl =
    if orig == search
    then repl
    else case orig of
           ELit _ -> orig
           EAnd a b -> replace a search repl `EAnd` replace b search repl
           EOr a b -> replace a search repl `EOr` replace b search repl
           EEff a b -> EEff (replace a search repl) (replace b search repl)
           ENot a -> ENot $ replace a search repl

exprAtoms :: Expr -> S.Set Atom
exprAtoms expr =
    case expr of
      ELit (LAtom a) -> S.singleton a
      ELit _ -> S.empty
      EAnd a b -> exprAtoms a `S.union` exprAtoms b
      EOr a b -> exprAtoms a `S.union` exprAtoms b
      EEff a b -> exprAtoms a `S.union` exprAtoms b
      ENot a -> exprAtoms a

epc :: Expr -> Expr -> Expr
epc lit' expr' =
    simplify $
    if lit == expr
    then ELit (LBool True)
    else case expr of
           ELit _ -> ELit (LBool False)
           EAnd e e' -> epc lit e .|| epc lit e'
           EEff x e -> epc lit e .&& x
           EOr a b ->
               epc lit $
               ENot $
               (ENot $ a .&& a ) .&& (ENot $ b .&& b)
           ENot q ->
               case q of
                 EAnd a b -> epc lit $ ENot a .|| ENot b -- de morgan
                 EOr a b -> epc lit $ ENot a .&& ENot b -- de morgan
                 EEff _x _e -> error "Unhandled case! NOT (x > e)" -- what should we do here?
                 ELit _ -> ELit (LBool False)
                 ENot _ -> error "This should never happen (simplifier!)"
    where
      lit = simplify lit'
      expr = simplify expr'

contradiction :: Expr -> Bool
contradiction e =
    case simplify e of
      ELit _ -> False
      EAnd a (ENot b) -> a == b
      EAnd (ENot a) b -> a == b
      EAnd a b -> contradiction a || contradiction b
      EOr a b -> contradiction a || contradiction b
      ENot a -> contradiction a
      EEff x ee -> contradiction x || contradiction ee

simplify :: Expr -> Expr
simplify = loop
    where
      loop t =
          let res = simplify' t
          in if res == t then res else loop res

simplify' :: Expr -> Expr
simplify' e =
    case e of
      EOr a b
          | a == b -> simplify a
          | a == ELit (LBool True) || b == ELit (LBool True) -> ELit (LBool True)
          | a == ELit (LBool False) -> simplify b
          | b == ELit (LBool False) -> simplify a
          | a == ENot b -> ELit (LBool True)
          | ENot a == b -> ELit (LBool True)
          | otherwise -> EOr (simplify a) (simplify b)
      EAnd a b
          | a == b -> simplify a
          | a == ELit (LBool False) || b == ELit (LBool False) -> ELit (LBool False)
          | a == ELit (LBool True) -> simplify b
          | b == ELit (LBool True) -> simplify a
          | a == ENot b -> ELit (LBool False)
          | ENot a == b -> ELit (LBool False)
          | otherwise -> EAnd (simplify a) (simplify b)
      ENot (ENot a) -> simplify a
      ENot (ELit (LBool x)) -> ELit (LBool $ not x)
      ENot (EOr a b) -> EAnd (ENot a) (ENot b)
      ENot (EAnd a b) -> EOr (ENot a) (ENot b)
      ENot q -> ENot (simplify q)
      EEff a b
          | a == ELit (LBool True) -> simplify b
          | a == ELit (LBool False) -> ELit (LBool True)
          | otherwise -> EEff (simplify a) (simplify b)
      ELit _ -> e

regress :: Operator -> Expr -> Expr
regress (precond, eff) expr =
{-
    trace ("Atoms: " ++ show atoms ++ "\n"
           ++ "Parts are: "
           ++ show (contradiction precond) ++ ": "
              ++ prettyS precond ++ " <=> " ++ prettyS (simplify precond) ++ "\n"
           ++ show (contradiction exprR) ++ ": "
              ++ prettyS exprR ++ " <=> " ++ prettyS (simplify exprR) ++ "\n"
           ++ show (contradiction k) ++ ": " ++ prettyS k ++ " <=> " ++ prettyS (simplify k) ++ "\n"
           ++ "Thus: " ++ prettyS (precond .&& exprR .&& k) ++ " <=> "
              ++ (prettyS $ simplify $ precond .&& exprR .&& k) ++ "\n"
          ) $
-}
    simplify $ precond .&& exprR .&& k
    where
      atoms = S.toList $ exprAtoms expr
      exprR =
          foldl' exprRStep expr atoms
      exprRStep st atom =
          replace st (ELit $ LAtom atom) $
          EOr (epc (ELit $ LAtom atom) eff)
          (ELit (LAtom atom) `EAnd` (ENot $ epc (ENot $ ELit $ LAtom atom) eff))
      k =
          foldl' kStep (ELit $ LBool True) atoms
      kStep accum atom =
          EAnd accum $
          ENot $
          epc (ELit $ LAtom atom) eff .&& epc (ENot $ ELit $ LAtom atom) eff

exRHome :: Expr
exRHome = ELit $ LAtom $ Atom "romeo-at-home"
exJHome :: Expr
exJHome = ELit $ LAtom $ Atom "juliet-at-home"
exRWork :: Expr
exRWork = ELit $ LAtom $ Atom "romeo-at-work"
exRDance :: Expr
exRDance = ELit $ LAtom $ Atom "romeo-dancing"
exJDance :: Expr
exJDance = ELit $ LAtom $ Atom "juliet-dancing"

exOps :: [(String, Expr, Expr)]
exOps =
    [ ( "go-dancing"
      , exJHome
      , exJDance .&& (ENot exJHome) .&& (exRHome .> (exRDance .&& (ENot exRHome)))
      )
    , ( "go-work"
      , exRHome
      , exRWork .&& (ENot exRHome)
      )
    , ( "go-home"
      , exRWork
      , exRHome .&& (ENot exRWork)
      )
    ]

exStart :: Expr -> Bool
exStart x =
    x == (exRHome .&& exJHome)
    || x == (exRHome .|| exJHome)

exGoal :: Expr
exGoal = exJDance .&& exRHome

runAlgorithm' :: Expr -> [(String, Expr, Expr)] -> (Expr -> Bool) -> IO Bool
runAlgorithm' goal ops isStart =
    step 0 Nothing goal
    where
      step :: Int -> Maybe Int -> Expr -> IO Bool
      step !ctr ix c =
          do let isTrue = isStart c
                 varName =
                     "y_" ++ show ctr ++
                     case ix of
                        Just v -> show v
                        Nothing -> ""
             putStrLn (varName <> " = " <> prettyS c)
             if isTrue || ctr > 200
                 then do when isTrue $
                             putStrLn $ "Done after " ++ show (ctr + 1) ++ " steps: " <> prettyS c
                         return isTrue
                 else do let nextOps = zip ops [1..]
                             try [] = pure False
                             try (((desc, pre, pos), nix) : more) =
                                 do let res = regress (pre, pos) c
                                    putStr $ "Apply " <> desc <> ", result: " <> prettyS res
                                    case res of
                                        ELit (LBool False) ->
                                            do putStrLn " (bad)"
                                               try more
                                        _
                                            | res == c ->
                                              do putStrLn " (duplicate)"
                                                 try more
                                            | otherwise ->
                                              do putStrLn ""
                                                 r <- step (ctr+1) (Just nix) res
                                                 if r then pure True else try more
                         try nextOps

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Ai.Planning.GeneralRegTest where

import Test.Framework

import Ai.Planning.Types
import Ai.Planning.GeneralReg

atomA :: Atom
atomA = Atom "a"

atomB :: Atom
atomB = Atom "b"

atomC :: Atom
atomC = Atom "c"

ea :: Atom -> Expr
ea = ELit . LAtom

test_epc :: IO ()
test_epc =
    do assertEqual (ELit $ LBool False) $
           epc (LAtom atomA) (ea atomB `EAnd` ea atomC)
       assertEqual (ELit $ LBool True) $
           epc (LAtom atomA) (ea atomA `EAnd` (ea atomB `EEff` ea atomA))
       assertEqual (ea atomC `EOr` ea atomB) $
           epc (LAtom atomA) ((ea atomC `EEff` ea atomA) `EAnd` (ea atomB `EEff` ea atomA))

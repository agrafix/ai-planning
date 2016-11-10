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

atomD :: Atom
atomD = Atom "d"

ea :: Atom -> Expr
ea = ELit . LAtom

test_epc :: IO ()
test_epc =
    do assertEqual (ELit $ LBool False) $
           epc (ea atomA) (ea atomB .&& ea atomC)
       assertEqual (ELit $ LBool True) $
           epc (ea atomA) (ea atomA .&& (ea atomB .> ea atomA))
       assertEqual (ea atomC .|| ea atomB) $
           epc (ea atomA) ((ea atomC .> ea atomA) .&& (ea atomB .> ea atomA))

sampleE :: Expr
sampleE =
    (ea atomB .> ea atomA)
    .&& (ea atomC .> (ENot $ ea atomA))
    .&& ea atomB
    .&& (ENot $ ea atomD)

sampleEpc :: Atom -> Expr
sampleEpc a =
    simplify $
    epc (ea a) sampleE .|| (ea a .&& (ENot $ epc (ENot $ ea a) sampleE))

test_epc2 :: IO ()
test_epc2 =
    assertEqual (ea atomB .|| (ea atomA .&& (ENot $ ea atomC))) (sampleEpc atomA)

test_epc2Contra :: IO ()
test_epc2Contra =
    assertBool (not $ contradiction $ sampleEpc atomA)

test_epc3 :: IO ()
test_epc3 =
    assertEqual (ELit $ LBool True) (sampleEpc atomB)

test_epc4 :: IO ()
test_epc4 =
    assertEqual (ea atomC) (sampleEpc atomC)

test_epc5 :: IO ()
test_epc5 =
    assertEqual (ELit $ LBool False) (sampleEpc atomD)

test_regress1 :: IO ()
test_regress1 =
    assertEqual (ea atomA) $
    regress (ea atomA, ea atomB) (ea atomB)

test_regress2 :: IO ()
test_regress2 =
    assertEqual (ea atomA .&& ea atomC .&& ea atomD) $
    regress (ea atomA, ea atomB) (ea atomB .&& ea atomC .&& ea atomD)

test_regress3 :: IO ()
test_regress3 =
    assertEqual (ea atomA .&& (ea atomC .|| ea atomB)) $
    regress (ea atomA, ea atomC .> ea atomB) (ea atomB)

test_regress4 :: IO ()
test_regress4 =
    assertEqual (ea atomA .&& ea atomC .&& (ENot $ ea atomB)) $
    let eff =
            (ea atomC .> ea atomB) .&& (ea atomB .> (ENot $ ea atomB))
    in regress (ea atomA, eff) (ea atomB)

test_regress5 :: IO ()
test_regress5 =
    assertEqual (ea atomA .&& (ea atomC .|| ea atomB) .&& (ENot $ ea atomD)) $
    let eff =
            (ea atomC .> ea atomB) .&& (ea atomD .> (ENot $ ea atomB))
    in regress (ea atomA, eff) (ea atomB)


test_simplify1 :: IO ()
test_simplify1 =
    assertEqual expect (simplify example)
    where
      expect =
          ELit (LBool True)
      example =
          EAnd
            (ELit (LBool True))
            (ENot (EAnd (ELit (LBool True)) (ELit (LBool False))))

test_simplify2 :: IO ()
test_simplify2 =
    assertEqual expect (simplify example)
    where
      expect =
          ELit (LBool True)
      example =
          ENot (EAnd (ELit (LBool True)) (ELit (LBool False)))

test_simplify3 :: IO ()
test_simplify3 =
    assertEqual expect (simplify example)
    where
      expect =
          ELit (LBool False)
      example =
          EAnd (ELit (LBool True)) (ELit (LBool False))

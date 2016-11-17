{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
module Ai.Planning.StripsReg where

import Ai.Planning.Types

import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad

data SExpr
    = SAtom !Atom
    | SNotAtom !Atom
    deriving (Show, Eq, Ord)

data Conj
    = Conj (S.Set SExpr)
    | CTrue
    | CFalse
    deriving (Show, Eq)

conjToSet :: Conj -> Maybe (S.Set SExpr)
conjToSet x =
    case x of
      Conj set -> Just set
      _ -> Nothing

conjToList :: Conj -> Maybe [SExpr]
conjToList x = S.toList <$> conjToSet x

instance Monoid Conj where
    mempty = CTrue
    q `mappend` p =
        case (q, p) of
            (_, CFalse) -> CFalse
            (CFalse, _) -> CFalse
            (CTrue, x) -> x
            (x, CTrue) -> x
            (Conj a, Conj b) ->
                let union = S.union a b
                    atoms = S.fromList $ getAtoms (S.toList union)
                    natoms = S.fromList $ getNAtoms (S.toList union)
                    contradiction = atoms `S.intersection` natoms
                in if S.null contradiction
                   then Conj union
                   else CFalse

mkConj :: S.Set SExpr -> Conj
mkConj x = Conj x <> Conj x

getAtoms :: [SExpr] -> [Atom]
getAtoms = mapMaybe m
    where
      m y =
          case y of
            SAtom x -> Just x
            _ -> Nothing

getNAtoms :: [SExpr] -> [Atom]
getNAtoms = mapMaybe m
    where
      m y =
          case y of
            SNotAtom x -> Just x
            _ -> Nothing

data Operation
    = Operation
    { o_name :: !T.Text
    , o_pre :: !Conj
    , o_post :: !Conj
    } deriving (Show, Eq)

atom :: T.Text -> SExpr
atom = SAtom . Atom

natom :: T.Text -> SExpr
natom = SNotAtom . Atom

evaluate :: M.Map Atom Bool -> Conj -> Bool
evaluate env conjs =
    case conjs of
      CFalse -> False
      CTrue -> True
      Conj y -> loop $ S.toList y
    where
      loop (x : xs) =
          case x of
            SAtom at ->
                case M.lookup at env of
                  Nothing -> error ("Missing " ++ show at ++ " in env!")
                  Just val -> val && loop xs
            SNotAtom at ->
                case M.lookup at env of
                  Nothing -> error ("Missing " ++ show at ++ " in env!")
                  Just val -> not val && loop xs
      loop [] = True

possibleOperations :: Conj -> [Operation] -> [Operation]
possibleOperations conj ops =
    case conj of
      CFalse -> []
      CTrue -> []
      Conj x ->
          filter (\op -> not . any (makesFalse op) . getAtoms . S.toList $ x) ops

applyOperation :: Conj -> Operation -> Conj
applyOperation conj op =
    case conj of
      CFalse -> CFalse
      CTrue -> CTrue
      Conj c ->
          let atoms = getAtoms . fromMaybe [] . conjToList . o_post $ op
              conj' =
                  Conj $ c `S.difference` (S.fromList $ map SAtom atoms)
          in o_pre op <> conj'

makesFalse :: Operation -> Atom -> Bool
makesFalse op atm =
    SNotAtom atm `elem` (fromMaybe S.empty $ conjToSet $ o_post op)
    && SAtom atm `elem` (fromMaybe S.empty $ conjToSet $ o_post op) -- todo: is needed ?

isSubset :: Conj -> Conj -> Bool
isSubset a b =
    case (a, b) of
      (Conj x, Conj y) ->
          x `S.isSubsetOf` y
      _ -> False

latexConj :: Conj -> T.Text
latexConj conj =
    case conj of
      CFalse -> "\\bot"
      CTrue -> "\\top"
      Conj x ->
          T.intercalate " \\land " $
          map handle (S.toList x)
    where
      handle t =
          case t of
            SAtom (Atom a) -> a
            SNotAtom (Atom a) -> "\\neg " <> a

runAlgorithm :: Conj -> [Operation] -> M.Map Atom Bool -> IO ()
runAlgorithm conj ops env =
    step 0 Nothing conj
    where
      step :: Int -> Maybe Int -> Conj -> IO ()
      step !ctr ix c =
          do let isTrue = evaluate env c
                 varName =
                     "y_" ++ show ctr ++
                     case ix of
                        Just v -> show v
                        Nothing -> ""
             putStrLn (varName <> " = " ++ T.unpack (latexConj c))
             if isTrue || ctr > 100
                 then putStrLn $ "Done after " ++ show (ctr + 1) ++ " steps: " <> T.unpack (latexConj c)
                 else do let nextOps = zip (possibleOperations c ops) [1..]
                         forM_ nextOps $ \(op, nix) ->
                             do let res = applyOperation c op
                                putStr $
                                    "\\text{sregr}_{o_" <> T.unpack (o_name op) <> "}(" <> varName <> ") = "
                                    <> T.unpack (latexConj res)
                                case res of
                                  CFalse ->
                                      do putStrLn " \\unicode{x21af}"
                                         pure ()
                                  _ ->
                                      if res `isSubset` c
                                      then do putStrLn " \\text{ (is subset)}"
                                              pure ()
                                      else do putStrLn ""
                                              step (ctr+1) (Just nix) res

exInitState :: M.Map Atom Bool
exInitState =
    M.fromList
    [ (Atom "a", False)
    , (Atom "b", True)
    , (Atom "c", False)
    , (Atom "d", True)
    , (Atom "e", True)
    ]

exGoalState :: Conj
exGoalState = mkConj [atom "a", atom "d"]

exOperations :: [Operation]
exOperations =
    [ Operation "1" (mkConj [atom "b", atom "d"]) (mkConj [atom "c", atom "e", natom "d"])
    , Operation "2" (mkConj [atom "b"]) (mkConj [atom "a", natom "c", natom "d"])
    , Operation "3" (mkConj [atom "a"]) (mkConj [atom "d"])
    ]

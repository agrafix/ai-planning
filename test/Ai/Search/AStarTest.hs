{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Ai.Search.AStarTest where

import Test.Framework

import Ai.Search.AStar
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.OrdPSQ as P

data GameField
    = GameFieldEmpty
    | GameFieldBlack
    | GameFieldWhite
    deriving (Show, Ord, Eq)

printField :: GameField -> T.Text
printField gf =
    case gf of
      GameFieldEmpty -> "\\text{e}"
      GameFieldBlack -> "\\text{b}"
      GameFieldWhite -> "\\text{w}"

type GameState = M.Map Int GameField

printGameState :: GameState -> T.Text
printGameState gs =
    "\\{"
    <> T.intercalate ", " (map showFld fields)
    <> "\\}"
    where
      showFld f =
          case M.lookup f gs of
            Nothing -> "\\text{?}"
            Just x -> printField x

fields :: [Int]
fields = [1, 2, 3, 4, 5]

initState :: GameState
initState =
    M.fromList
    [ (1, GameFieldBlack)
    , (2, GameFieldBlack)
    , (3, GameFieldWhite)
    , (4, GameFieldWhite)
    , (5, GameFieldEmpty)
    ]

atGoal :: GameState -> Bool
atGoal gs =
    loop 0 $ map snd $ M.toList gs
    where
      loop :: Int -> [GameField] -> Bool
      loop wctr [] = wctr == 2
      loop wctr (x : xs) =
          case x of
            GameFieldBlack -> wctr == 2
            GameFieldWhite -> loop (wctr + 1) xs
            GameFieldEmpty -> loop wctr xs

test_atGoal :: IO ()
test_atGoal =
    do assertBool (not $ atGoal initState)
       assertBool (atGoal sampleGoal)
    where
        sampleGoal =
            M.fromList
            [ (1, GameFieldWhite)
            , (2, GameFieldWhite)
            , (3, GameFieldBlack)
            , (4, GameFieldBlack)
            , (5, GameFieldEmpty)
            ]

heurFun :: GameState -> Score Int
heurFun gs =
    case blackFields of
        [(p1, _), (p2, _)] ->
            let getWs ref =
                    length $
                    filter (\(p, n) -> n == GameFieldWhite && p > ref) $ M.toList gs
            in ScoreValue $ getWs p1 + getWs p2
        _ -> ScoreInfinity -- BAD STATE
    where
      blackFields =
          filter (\x -> snd x == GameFieldBlack) $
          M.toList gs

test_heurFun :: IO ()
test_heurFun = assertEqual 4 (heurFun initState)

costFun :: GameState -> GameState -> Score Int
costFun gs gs' =
    case changedCells of
       [(pos, _), (pos', _)] -> ScoreValue (abs $ pos - pos')
       _ -> ScoreInfinity
    where
      changedCells =
          map (\((pos, _), (pos', _)) -> (pos, pos')) $
          filter (\((_, val), (_, val')) -> val /= val') $
          zip (M.toList gs) (M.toList gs')

nextStates :: GameState -> S.Set GameState
nextStates gs =
    S.fromList $
    flip concatMap fields $ \fld ->
    catMaybes $
    concatMap (\op -> [moveIfEmpty (\x -> x + op) fld, moveIfEmpty (\x -> x - op) fld])
    [1, 2, 3]
    where
      moveIfEmpty op fld =
          do myVal <- M.lookup fld gs
             if M.lookup (op fld) gs == Just GameFieldEmpty
             then Just $ M.insert fld GameFieldEmpty $ M.insert (op fld) myVal gs
             else Nothing

astarCfg :: AStarConfig GameState Int
astarCfg =
    AStarConfig
    { ac_nextStates = nextStates
    , ac_costFunction = costFun
    , ac_heuristicFunction = heurFun
    , ac_atGoal = atGoal
    }

printScore :: Score Int -> T.Text
printScore s =
    case s of
      ScoreInfinity -> "\\infty"
      ScoreValue v -> T.pack $ show v

printOpen :: P.OrdPSQ GameState (Score Int) () -> T.Text
printOpen q =
    "["
    <> T.intercalate ", " (map printEl $ P.toList q)
    <> "]"
    where
      printEl (gs, val, ()) =
          "(" <> printGameState gs <> "" <> "," <> printScore val <> ")"

hook :: GameState -> Score Int -> AStarSt GameState Int -> IO ()
hook gs score st =
    do putStrLn $ "\\text{node}_\\text{current} & = & " <> (T.unpack $ printGameState gs) <> "\\\\"
       putStrLn $ "\\text{score}_\\text{current} & = & " <> (T.unpack $ printScore score) <> "\\\\"
       putStrLn $ "\\text{open}  & = & " <> (T.unpack $ printOpen $ as_open st) <> "\\\\"

test_astar :: IO ()
test_astar =
    do res <- astarM' astarCfg hook initState
       _ <- assertJust res
       pure ()

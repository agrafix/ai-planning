{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Ai.Search.EnforcedHillClimbTest where

import Ai.Search.EnforcedHillClimb
import Ai.Search.Score

import Test.Framework
import Data.Monoid
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

data GameState
    = GameState
    { gs_hanselPos :: (Int, Int)
    , gs_gretelPos :: (Int, Int)
    } deriving (Show, Eq, Ord)

printGameState :: GameState -> T.Text
printGameState gs =
    "\\{"
    <> "\\text{hansel} = " <> (T.pack $ show $ gs_hanselPos gs)
    <> ", \\text{gretel} = " <> (T.pack $ show $ gs_gretelPos gs)
    <> "\\}"

data Movement
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    deriving (Show, Eq, Bounded, Enum)

move :: (Int, Int) -> Movement -> Maybe (Int, Int)
move (x, y) mv =
    if xa < 1 || xa > 5 || ya < 1 || ya > 5 || a `elem` blockedFields
    then Nothing
    else Just a
    where
      a@(xa, ya) =
          case mv of
            MoveUp -> (x, y + 1)
            MoveLeft -> (x - 1, y)
            MoveDown-> (x, y - 1)
            MoveRight -> (x + 1, y)

initState :: GameState
initState =
    GameState
    { gs_hanselPos = (1, 2)
    , gs_gretelPos = (4, 4)
    }

isGoal :: GameState -> Bool
isGoal gs =
    gs_hanselPos gs == goal && gs_gretelPos gs == goal

goal :: (Int, Int)
goal = (3, 3)

blockedFields :: [(Int, Int)]
blockedFields =
    [ (1, 5), (2, 5), (1, 4), (2, 4), (3, 4), (4, 3), (2, 1), (5, 1) ]

heurFun :: GameState -> Score Int
heurFun gs =
    let (hx, hy) = gs_hanselPos gs
        (gx, gy) = gs_gretelPos gs
        (tx, ty) = goal
    in ScoreValue $
       abs (tx - hx) + abs (ty - hy) + abs (tx - gx) + abs (ty - gy)

test_heurFun :: IO ()
test_heurFun =
    assertEqual (heurFun initState) 5

nextStates :: GameState -> S.Set GameState
nextStates gs =
    S.fromList $
    flip concatMap [minBound .. maxBound] $ \mv ->
    catMaybes $
    [hansel mv, gretel mv]
    where
      gretel mv =
          do pos <- move (gs_gretelPos gs) mv
             pure $
                 gs
                 { gs_gretelPos = pos
                 }
      hansel mv =
          do pos <- move (gs_hanselPos gs) mv
             pure $
                 gs
                 { gs_hanselPos = pos
                 }

test_nextStates :: IO ()
test_nextStates =
    assertEqual (nextStates s) n
    where
      s = GameState {gs_hanselPos = (3,3), gs_gretelPos = (4,2)}
      n =
          S.fromList
          [ GameState{gs_hanselPos = (2, 3), gs_gretelPos = (4, 2)}
          , GameState{gs_hanselPos = (3, 2), gs_gretelPos = (4, 2)}
          , GameState{gs_hanselPos = (3, 3), gs_gretelPos = (3, 2)}
          , GameState{gs_hanselPos = (3, 3), gs_gretelPos = (4, 1)}
          , GameState{gs_hanselPos = (3, 3), gs_gretelPos = (5, 2)}
          ]

printScore :: Score Int -> T.Text
printScore s =
    case s of
      ScoreInfinity -> "\\infty"
      ScoreValue v -> T.pack $ show v

hook :: StepHook GameState Int IO
hook node st =
    do T.putStr (printGameState node)
       T.putStr " & "
       T.putStr $ "h = " <> (printScore $ heurFun node)
       T.putStr " & "
       T.putStr ("h_0 = " <> (printScore $ ss_bestHeur st))
       T.putStrLn "\\\\"

cfg :: EHCConfig GameState Int
cfg =
    EHCConfig
    { ec_nextStates = nextStates
    , ec_heuristicFunction = heurFun
    , ec_atGoal = isGoal
    }

test_ehc :: IO ()
test_ehc =
    do x <- ehcM' cfg hook initState
       res <- assertJust x
       assertEqual (gs_gretelPos res) goal
       assertEqual (gs_hanselPos res) goal

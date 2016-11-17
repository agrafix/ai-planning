module Ai.Search.Score
    ( ScoreMap
    , emptyScoreMap, setScore, setScoreV, getScore
    , Score(..)
    )
where

import Data.Maybe
import qualified Data.Map as M

newtype ScoreMap n c
    = ScoreMap { _unScoreMap :: M.Map n (Score c) }
    deriving (Show, Eq)

data Score c
    = ScoreInfinity
    | ScoreValue c
    deriving (Show, Eq)

instance Functor Score where
    fmap f s =
        case s of
          ScoreInfinity -> ScoreInfinity
          ScoreValue c -> ScoreValue (f c)

instance Num c => Num (Score c) where
    x + y =
        case (x, y) of
          (ScoreInfinity, _) -> ScoreInfinity
          (_, ScoreInfinity) -> ScoreInfinity
          (ScoreValue q, ScoreValue p) -> ScoreValue (q + p)
    x - y =
        case (x, y) of
          (ScoreInfinity, _) -> ScoreInfinity
          (_, ScoreInfinity) -> ScoreInfinity
          (ScoreValue q, ScoreValue p) -> ScoreValue (q - p)
    x * y =
        case (x, y) of
          (ScoreInfinity, _) -> ScoreInfinity
          (_, ScoreInfinity) -> ScoreInfinity
          (ScoreValue q, ScoreValue p) -> ScoreValue (q * p)
    abs = fmap abs
    signum x =
        case x of
          ScoreInfinity -> 0
          ScoreValue q -> ScoreValue (signum q)
    fromInteger = ScoreValue . fromInteger
    negate = fmap negate

instance Ord c => Ord (Score c) where
    x <= y =
        case (x, y) of
          (ScoreInfinity, ScoreInfinity) -> True
          (ScoreInfinity, _) -> False
          (_, ScoreInfinity) -> False
          (ScoreValue q, ScoreValue p) -> q <= p

emptyScoreMap :: ScoreMap n c
emptyScoreMap = ScoreMap M.empty

setScore :: Ord n => n -> c -> ScoreMap n c -> ScoreMap n c
setScore n c m = setScoreV n (ScoreValue c) m

setScoreV :: Ord n => n -> Score c -> ScoreMap n c -> ScoreMap n c
setScoreV n sv (ScoreMap m) = ScoreMap $ M.insert n sv m

getScore :: Ord n => n -> ScoreMap n c -> Score c
getScore n (ScoreMap m) = fromMaybe ScoreInfinity $ M.lookup n m

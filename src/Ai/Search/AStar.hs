module Ai.Search.AStar where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.List
import qualified Data.OrdPSQ as P
import qualified Data.Set as S
import qualified Data.Map as M

newtype ScoreMap n c
    = ScoreMap { unScoreMap :: M.Map n (Score c) }
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


data AStarSt n c
    = AStarSt
    { as_closed :: !(S.Set n)
    , as_open :: !(P.OrdPSQ n (Score c) ())
    , as_gScore :: !(ScoreMap n c)
    , as_prevState :: !(M.Map n n)
    } deriving (Show, Eq)

data AStarConfig n c
    = AStarConfig
    { ac_nextStates :: n -> S.Set n
    , ac_costFunction :: n -> n -> Score c
    , ac_heuristicFunction :: n -> Score c
    , ac_atGoal :: n -> Bool
    }

astar ::
    (Ord n, Ord c, Num c)
    => AStarConfig n c -> n -> Maybe (M.Map n n)
astar cfg startNode = runIdentity $ astarM cfg startNode

astarM ::
    (Monad m, Ord n, Ord c, Num c)
    => AStarConfig n c -> n -> m (Maybe (M.Map n n))
astarM cfg startNode = astarM' cfg (\_ _ _ -> pure ()) startNode

type StepHook n c m =
    n -> Score c -> AStarSt n c -> m ()

astarM' ::
    (Monad m, Ord n, Ord c, Num c)
    => AStarConfig n c -> StepHook n c m -> n -> m (Maybe (M.Map n n))
astarM' cfg stepHook startNode =
    runMaybeT $
    loop initState
    where
        loop st =
            do (cnode, score, (), open') <-
                   MaybeT $ pure $
                   P.minView (as_open st)
               if ac_atGoal cfg cnode
                  then pure (as_prevState st)
                  else do let st' = st { as_open = open' }
                              st'' = step cfg cnode score st'
                          lift $ stepHook cnode score st''
                          loop st''
        initState =
            AStarSt
            { as_closed = S.empty
            , as_open = P.insert startNode (ac_heuristicFunction cfg startNode) () P.empty
            , as_gScore = setScore startNode 0 emptyScoreMap
            , as_prevState = M.empty
            }

step ::
    (Ord n, Ord c, Num c)
    => AStarConfig n c
    -> n
    -> Score c
    -> AStarSt n c
    -> AStarSt n c
step cfg currentNode currentScore stN =
    let stFilter (n, tentativeG) =
            not (P.member n (as_open stN))
            || tentativeG < getScore n (as_gScore stN)
        goodStates =
            filter stFilter $
            map (\n -> (n, currentScore + ac_costFunction cfg currentNode n)) $
            S.toList $
            possibleStates
        possibleStates =
            (ac_nextStates cfg currentNode) `S.difference` (as_closed stN)
        foldIntoState st (n, tentativeG) =
            let gScore =
                    setScoreV n tentativeG (as_gScore st)
                fScore = tentativeG + ac_heuristicFunction cfg n
            in st
               { as_gScore = gScore
               , as_prevState = M.insert n currentNode (as_prevState st)
               , as_open =
                    if not (P.member n (as_open st))
                    then P.insert n fScore () (as_open st)
                    else as_open st
               }
    in flip (foldl' foldIntoState) goodStates $
       stN
       { as_closed = S.insert currentNode (as_closed stN)
       }

module Ai.Search.EnforcedHillClimb where

import Ai.Search.Score

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as S

data EHCSt n c
    = EHCSt
    { ss_open :: !(Seq.Seq n)
    , ss_closed :: !(S.Set n)
    , ss_bestHeur :: !(Score c)
    }

data EHCConfig n c
    = EHCConfig
    { ec_nextStates :: n -> S.Set n
    , ec_heuristicFunction :: n -> Score c
    , ec_atGoal :: n -> Bool
    }

type StepHook n c m = n -> EHCSt n c -> m ()

ehc ::
    (Ord c, Ord n)
    => EHCConfig n c -> n -> Maybe n
ehc cfg startNode =
    runIdentity $ ehcM cfg startNode

ehcM ::
    (Monad m, Ord c, Ord n)
    => EHCConfig n c -> n -> m (Maybe n)
ehcM cfg startNode = ehcM' cfg (\_ _ -> pure ()) startNode

ehcM' ::
    (Monad m, Ord c, Ord n)
    => EHCConfig n c -> StepHook n c m -> n -> m (Maybe n)
ehcM' cfg hook startNode =
    runMaybeT $
    if ec_atGoal cfg startNode
    then pure startNode
    else loop initState
    where
      loop st =
          case Seq.viewl (ss_open st) of
            Seq.EmptyL -> empty
            x Seq.:< open' ->
                if S.member x (ss_closed st)
                then loop (st { ss_open = open' })
                else do let stNode =
                                st
                                { ss_open = open'
                                , ss_closed = S.insert x (ss_closed st)
                                }
                        lift $ hook x stNode
                        case step cfg x stNode of
                          Left goalNode ->
                              pure goalNode
                          Right nextState ->
                              loop nextState
      initState =
          EHCSt
          { ss_open = Seq.singleton startNode
          , ss_closed = S.empty
          , ss_bestHeur = ec_heuristicFunction cfg startNode
          }

step ::
    Ord c
    => EHCConfig n c
    -> n
    -> EHCSt n c
    -> Either n (EHCSt n c)
step cfg currentNode stN =
    let nextStates =
            ec_nextStates cfg currentNode
    in nsLoop stN $ S.toList nextStates
    where
      nsLoop st [] = Right st
      nsLoop st (x : xs) =
          if ec_atGoal cfg x
          then Left x
          else let h = ec_heuristicFunction cfg x
               in if h < ss_bestHeur st
                  then Right $
                       st { ss_open = Seq.singleton x, ss_bestHeur = h }
                  else let st' =
                               st { ss_open = ss_open st Seq.|> x }
                       in nsLoop st' xs

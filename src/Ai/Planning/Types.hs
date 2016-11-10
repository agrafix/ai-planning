module Ai.Planning.Types
    ( Atom(..) )
where

import qualified Data.Text as T

newtype Atom
    = Atom { unAtom :: T.Text }
    deriving (Show, Eq, Ord)

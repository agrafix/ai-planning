module Ai.Planning.Types
    ( Atom(..)
    , PrettyText(..)
    , prettyS
    )
where

import qualified Data.Text as T

newtype Atom
    = Atom { unAtom :: T.Text }
    deriving (Show, Eq, Ord)

instance PrettyText Atom where
    prettyT (Atom t) = t

class PrettyText t where
    prettyT :: t -> T.Text

prettyS :: PrettyText t => t -> String
prettyS = T.unpack . prettyT

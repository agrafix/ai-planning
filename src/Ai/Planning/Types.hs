{-# LANGUAGE OverloadedStrings #-}
module Ai.Planning.Types
    ( Atom(..)
    , PrettyText(..)
    , prettyS
    , PrettyLatex(..)
    , prettySL
    )
where

import Data.Monoid
import qualified Data.Text as T

newtype Atom
    = Atom { unAtom :: T.Text }
    deriving (Show, Eq, Ord)

instance PrettyText Atom where
    prettyT (Atom t) = t

instance PrettyLatex Atom where
    prettyL (Atom t) = "\\text{" <> t <> "}"

class PrettyText t where
    prettyT :: t -> T.Text

class PrettyLatex t where
    prettyL :: t -> T.Text

prettyS :: PrettyText t => t -> String
prettyS = T.unpack . prettyT

prettySL :: PrettyLatex t => t -> String
prettySL = T.unpack . prettyL

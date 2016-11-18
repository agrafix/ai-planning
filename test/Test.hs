{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Ai.Planning.GeneralRegTest
import {-@ HTF_TESTS @-} Ai.Search.AStarTest
import {-@ HTF_TESTS @-} Ai.Search.EnforcedHillClimbTest

main :: IO ()
main = htfMain htf_importedTests

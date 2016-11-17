{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Ai.Planning.GeneralRegTest
import {-@ HTF_TESTS @-} Ai.Search.AStarTest

main :: IO ()
main = htfMain htf_importedTests

{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Ai.Planning.GeneralRegTest

main :: IO ()
main = htfMain htf_importedTests

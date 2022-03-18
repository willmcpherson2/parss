module Main (main) where

import qualified Balanced
import qualified Not
import System.Exit
import Test.HUnit (Counts(errors, failures), Test(TestList), runTestTT)

main :: IO ()
main = do
  counts <- runTestTT $ TestList
    [ Not.tokenTests
    , Not.treeTests
    , Not.exprTests
    , Balanced.tokenTests
    , Balanced.parensTests
    ]
  if errors counts + failures counts == 0 then exitSuccess else exitFailure

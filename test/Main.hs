module Main (main) where

import qualified Abc
import qualified Balanced
import qualified Infinite
import qualified Lisp
import qualified Not
import qualified Stack
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
    , Abc.abcTests
    , Lisp.tokenTests
    , Lisp.treeTests
    , Lisp.exprTests
    , Infinite.tokenTests
    , Stack.programTests
    ]
  if errors counts + failures counts == 0 then exitSuccess else exitFailure

module Main (main) where

import System.Exit
import Test.HUnit (Counts(errors, failures), runTestTT)
import Token (tokenTests)

main :: IO ()
main = do
  counts <- runTestTT tokenTests
  if errors counts + failures counts == 0 then exitSuccess else exitFailure

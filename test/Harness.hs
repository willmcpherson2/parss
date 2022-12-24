module Harness (tests) where

import Parser
import Test.HUnit
  ( AssertionPredicable (assertionPredicate),
    Test (TestList),
    (~?),
  )

tests ::
  (Eq a, Show a, Show s) =>
  (s -> s') ->
  [(String, Parser s' a)] ->
  [(s, a)] ->
  Test
tests initInput namesAndParsers inputsAndOutputs = TestList $ do
  (input, output) <- inputsAndOutputs
  (name, parser) <- namesAndParsers
  let input' = initInput input
      result = parse parser input'
      message =
        unlines
          [ "parser: " ++ name,
            "input: " ++ show input,
            "expected: " ++ show output,
            "got: " ++ show result
          ]
      test = assertionPredicate (result == output) ~? message
  pure test

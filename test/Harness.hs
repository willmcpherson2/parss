module Harness (tests) where

import Test.HUnit
  ( AssertionPredicable (assertionPredicate),
    Test (TestList),
    (~?),
  )

tests :: (Eq b, Show a, Show b) => (a -> b) -> [(a, b)] -> Test
tests parser = TestList . map mkTest
  where
    mkTest (input, expected) =
      let result = parser input
          msg =
            unlines
              [ "input:",
                show input,
                "expected:",
                show expected,
                "result:",
                show result
              ]
       in assertionPredicate (result == expected) ~? msg

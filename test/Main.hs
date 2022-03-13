module Main (main) where

import Combinators
import Parser
import System.Exit
import Test.HUnit
import Util

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0 then exitSuccess else exitFailure

tests :: Test
tests = TestList
  [ parse parseParens "" ~?= []
  , parse parseParens "()" ~?= [Open, Close]
  , parse parseParens "(())" ~?= [Open, Open, Close, Close]
  , parse parseParens "(a)" ~?= [Open, ParenErr, Close]
  , parse parseABs (withPos "") ~?= []
  , parse parseABs (withPos "a") ~?= [A 0]
  , parse parseABs (withPos "b") ~?= [B 0]
  , parse parseABs (withPos "ab") ~?= [A 0, B 1]
  , parse parseABs (withPos "bab") ~?= [B 0, A 1, B 2]
  ]

data Paren = Open | Close | ParenErr
  deriving (Eq, Show)

parseParens :: Parser String [Paren]
parseParens = star $ takeToken <||> \case
  '(' -> Open
  ')' -> Close
  _ -> ParenErr

data AB = A Int | B Int | ABErr Int
  deriving (Eq, Show)

parseABs :: Parser [(Int, Char)] [AB]
parseABs = star $ takeToken <||> \case
  (pos, 'a') -> A pos
  (pos, 'b') -> B pos
  (pos, _) -> ABErr pos

module Main (main) where

import Combinators
import Data.Functor ((<&>))
import Parser
import System.Exit
import Test.HUnit

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
  ]

data Paren = Open | Close | ParenErr
  deriving (Eq, Show)

parseParens :: Parser String [Paren]
parseParens = star parseParen

parseParen :: Parser String (Maybe Paren)
parseParen = takeToken <&> \case
  Just '(' -> Just Open
  Just ')' -> Just Close
  Just _ -> Just ParenErr
  Nothing -> Nothing

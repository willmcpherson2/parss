module Main (main) where

import Parser
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  counts2 <- runTestTT tests
  if errors counts2 + failures counts2 == 0 then exitSuccess else exitFailure

tests :: Test
tests = TestList
  [ runParser parseParens "" ~?= []
  , runParser parseParens "()" ~=? [Open, Close]
  , runParser parseParens "(())" ~=? [Open, Open, Close, Close]
  , runParser parseParens "(a)" ~=? [Open, ParenErr, Close]
  ]

data Paren = Open | Close | ParenErr
  deriving (Eq, Show)

parseParens :: Parser String [Paren]
parseParens = parseParen >>= maybe (pure []) (\x -> (x :) <$> parseParens)

parseParen :: Parser String (Maybe Paren)
parseParen = Parser $ \input -> case stream input of
  Left input -> (input, Nothing)
  Right (input, token) -> case token of
    '(' -> (input, Just Open)
    ')' -> (input, Just Close)
    _ -> (input, Just ParenErr)

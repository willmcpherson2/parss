module Main (main) where

import Combinators
import Control.Monad (void)
import Data.Char (isAlpha)
import Data.List.NonEmpty (NonEmpty((:|)))
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
  [ parse parseTokens "" ~?= []
  , parse parseTokens "()" ~?= [Open, Close]
  , parse parseTokens "(())" ~?= [Open, Open, Close, Close]
  , parse parseTokens "(a)" ~?= [Open, Name ('a' :| []), Close]
  , parse parseTokens "foo()" ~?= [Name ('f' :| "oo"), Open, Close]
  , parse parseTokens "(1)" ~?= [Open, TokenErr, Close]
  , parse parseTokens " " ~?= []
  , parse parseTokens " ( foo ) " ~?= [Open, Name ('f' :| "oo"), Close]
  , parse parseTokens " ( 12 ) " ~?= [Open, TokenErr, TokenErr, Close]
  ]

data Token
  = Open
  | Close
  | Name (NonEmpty Char)
  | TokenErr
  deriving (Eq, Show)

parseTokens :: Parser String [Token]
parseTokens =
  let
    skipSpace = void $ star $ try $ match ' '
    open = Open <<$ try (match '(')
    close = Close <<$ try (match ')')
    name = Name <<$>> plus (try $ satisfy isAlpha)
    err = TokenErr <<$ takeToken
    token = (open <<|>> close <<|>> name <<|>> err) <* skipSpace
    tokens = skipSpace *> star token
  in tokens

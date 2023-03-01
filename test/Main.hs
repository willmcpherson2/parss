module Main (main) where

import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Harness (tests)
import Parss.Combinators
import Parss.Parser
import Parss.Trans
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Test, errors, failures, runTestTT)
import Text.Read (readMaybe)
import Prelude hiding (or)

main :: IO ()
main = do
  counts <- runTestTT testProgram
  if errors counts + failures counts == 0 then exitSuccess else exitFailure

testProgram :: Test
testProgram =
  tests
    parseProgram
    [ ("", ExpErr ExpectedTree ("", "", "")),
      (" ", ExpErr ExpectedTree ("", "", " ")),
      ("(", ExpErr ParenNotClosed ("", "(", "")),
      ("(()", ExpErr ParenNotClosed ("", "(()", "")),
      ("x", ExpErr ExpectedNum ("", "x", "")),
      ("()", ExpErr BadOp ("", "()", "")),
      ("(())", ExpErr ExpectedOp ("", "(())", "")),
      ("(*)", ExpErr UndefinedOp ("(", "*", ")")),
      ( "(+ 1 2)",
        Add
          [ Num 1 (reverse "(+ ", "1", " 2)"),
            Num 2 (reverse "(+ 1 ", "2", ")")
          ]
          (reverse "", "(+ 1 2)", "")
      ),
      ( "(+ 1 (- 2 3))",
        Add
          [ Num 1 (reverse "(+ ", "1", " (- 2 3))"),
            Sub
              [ Num 2 (reverse "(+ 1 (- ", "2", " 3))"),
                Num 3 (reverse "(+ 1 (- 2 ", "3", "))")
              ]
              (reverse "(+ 1 ", "(- 2 3)", ")")
          ]
          (reverse "", "(+ 1 (- 2 3))", "")
      )
    ]

type Match = String

type Source = (String, String)

type Loc = (String, String, String)

data Err
  = ParenNotClosed
  | ExpectedTree
  | ExpectedNum
  | BadOp
  | ExpectedOp
  | UndefinedOp
  deriving (Eq, Show)

data Exp
  = Num Int Loc
  | Add [Exp] Loc
  | Sub [Exp] Loc
  | ExpErr Err Loc
  deriving (Eq, Show)

parseProgram :: String -> Exp
parseProgram = parseExp . parse parseTree . ("",)

parseExp :: Tree -> Exp
parseExp = \case
  Word s loc ->
    case readMaybe $ toList s of
      Just n -> Num n loc
      Nothing -> ExpErr ExpectedNum loc
  Parens trees loc ->
    case trees of
      op : args -> case op of
        Word op opLoc -> case op of
          '+' :| "" -> Add (map parseExp args) loc
          '-' :| "" -> Sub (map parseExp args) loc
          _ -> ExpErr UndefinedOp opLoc
        _ -> ExpErr ExpectedOp loc
      _ -> ExpErr BadOp loc
  TreeErr err loc -> ExpErr err loc

data Tree
  = Word (NonEmpty Char) Loc
  | Parens [Tree] Loc
  | TreeErr Err Loc
  deriving (Eq, Show)

parseTree :: Parser Match Source Tree
parseTree = try parseSubTree `orElse` locate (pure $ TreeErr ExpectedTree)

parseSubTree :: Parser Match Source (Maybe Tree)
parseSubTree = do
  skipSpace
  try parseParens `or` parseWord

parseParens :: Parser Match Source (Maybe Tree)
parseParens = locateM . fallible $ do
  need $ try $ is '('
  trees <- ok $ star parseSubTree
  ok skipSpace
  fallback (TreeErr ParenNotClosed) $ try $ is ')'
  pure $ Parens trees

parseWord :: Parser Match Source (Maybe Tree)
parseWord = locateM . fallible $ do
  chars <- need . plus . try . satisfy $ \char ->
    not (isSpace char) && char `notElem` "()"
  pure $ Word chars

skipSpace :: Parser Match Source String
skipSpace = star $ try $ satisfy isSpace

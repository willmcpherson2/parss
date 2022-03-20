module Balanced (tokenTests, parensTests) where

import Combinators
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Harness
import Parser
import Test.HUnit (Test)

parensTests :: Test
parensTests = tests
  (parse parseTokens)
  [("parseParens", parseParens)]
  [ ("()", Parens [])
  , ("(", ParensErr (Just Open))
  , (")", ParensErr (Just Close))
  , ("(())", Parens [Parens []])
  , ("(a)", Parens [ParensErr (Just UnexpectedToken)])
  , ("((", ParensErr (Just Open))
  , ("(()())", Parens [Parens [], Parens []])
  , ("((()))", Parens [Parens [Parens []]])
  ]

data Parens
  = Parens [Parens]
  | ParensErr (Maybe Token)
  deriving (Show, Eq)

parseParens :: Parser [Token] Parens
parseParens =
  let
    parens = try $ runMaybeT $ do
      Open <- MaybeT takeToken
      parens <- lift $ star $ try $ parens <<|>> tokenErr
      Close <- MaybeT takeToken
      pure $ Parens parens
    tokenErr = try $ runMaybeT $ do
      UnexpectedToken <- MaybeT takeToken
      pure $ ParensErr $ Just UnexpectedToken
    err = ParensErr <$> takeToken
  in parens <<|>> tokenErr |>> err

tokenTests :: Test
tokenTests = tests
  id
  [("parseTokens", parseTokens)]
  [("()", [Open, Close]), ("a", [UnexpectedToken])]

data Token
  = Open
  | Close
  | UnexpectedToken
  deriving (Show, Eq)

parseTokens :: Parser String [Token]
parseTokens =
  let
    open = runMaybeT $ do
      MaybeT $ try $ matchM '('
      pure Open
    close = runMaybeT $ do
      MaybeT $ try $ matchM ')'
      pure Close
    err = runMaybeT $ do
      MaybeT takeToken
      pure UnexpectedToken
    token = open <<|>> close <<|>> err
  in star token

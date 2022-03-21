{-# LANGUAGE OverloadedStrings #-}

module Stack (programTests) where

import Combinators
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Either.Extra (fromEither)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)), toList)
import Data.Text as T
import Harness (tests)
import Parser
import Test.HUnit (Test)
import Text.Read (readMaybe)

programTests :: Test
programTests = tests
  (parse parseTokens)
  [("parseProgram", parseProgram)]
  [ ("push x 1", Program [Expr Push (Name ('x' :| "")) (Val 1)])
  , ( "push foo 10\npop bar 3"
    , Program
      [ Expr Push (Name ('f' :| "oo")) (Val 10)
      , Expr Pop (Name ('b' :| "ar")) (Val 3)
      ]
    )
  , ( "blah x 1"
    , Program [Expr (OpErr (Alpha ('b' :| "lah"))) (Name ('x' :| "")) (Val 1)]
    )
  ]

newtype Program = Program [Expr]
  deriving (Show, Eq)

data Expr
  = Expr Op Name Val
  | ExpectedOp
  | ExpectedName
  | ExpectedVal
  deriving (Show, Eq)

data Op
  = Push
  | Pop
  | OpErr Token
  deriving (Show, Eq)

data Name
  = Name (NonEmpty Char)
  | NameErr Token
  deriving (Show, Eq)

data Val
  = Val Int
  | ValErr Token
  deriving (Show, Eq)

parseProgram :: Parser [Token] Program
parseProgram =
  let
    parseOp = takeToken <&> \case
      Just (Alpha ('p' :| "ush")) -> Just Push
      Just (Alpha ('p' :| "op")) -> Just Pop
      Just t -> Just $ OpErr t
      Nothing -> Nothing
    parseName = takeToken <&> \case
      Just (Alpha s) -> Just $ Name s
      Just t -> Just $ NameErr t
      Nothing -> Nothing
    parseVal = takeToken <&> \case
      Just (Digit i) -> Just $ Val i
      Just t -> Just $ ValErr t
      Nothing -> Nothing
    expr = runMaybeT . fmap fromEither . runExceptT $ do
      op <- lift $ MaybeT parseOp
      name <- ExceptT $ lift $ parseName !>> ExpectedName
      val <- ExceptT $ lift $ parseVal !>> ExpectedVal
      pure $ Expr op name val
  in do
    exprs <- star expr
    pure $ Program exprs

--------------------------------------------------------------------------------

data Token
  = Alpha (NonEmpty Char)
  | Digit Int
  | TokenErr Char
  deriving (Show, Eq)

parseTokens :: Parser Text [Token]
parseTokens =
  let
    skipSpaces = void $ star $ try $ satisfyM isSpace
    alpha = runMaybeT $ Alpha <$> MaybeT (plus $ try $ satisfyM isAlpha)
    digit = runMaybeT $ do
      s <- MaybeT $ plus $ try $ satisfyM isDigit
      i <- MaybeT $ pure $ readMaybe $ toList s
      pure $ Digit i
    err = runMaybeT $ TokenErr <$> MaybeT takeToken
    token = do
      t <- try alpha <<|>> try digit <<|>> err
      skipSpaces
      pure t
  in do
    skipSpaces
    star token

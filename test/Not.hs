{-# LANGUAGE TypeFamilies #-}

module Not (tokenTests, treeTests, exprTests) where

import Combinators
import Control.Arrow ((<<<))
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Char (isAlpha)
import Data.List.NonEmpty (NonEmpty((:|)))
import Harness (tests)
import Parser
import Stream
import Test.HUnit (Test)
import Util (parse)

exprTests :: Test
exprTests = tests
  (\s -> parse (parseTree <<< parseTokens) (0, s))
  [("parseExpr", parseExpr)]
  [ ("", ExprErr (Just (TreeErr Nothing)))
  , ("true", T 0)
  , ("false", F 0)
  , ("(not true)", Not 0 (T 5))
  , ("(not (not false))", Not 0 (Not 5 (F 10)))
  ]

data Expr
  = T Int
  | F Int
  | Not Int Expr
  | ExprErr (Maybe Tree)
  deriving (Eq, Show)

parseExpr :: Parser Tree Expr
parseExpr =
  let
    t = runMaybeT $ do
      Leaf pos ('t' :| "rue") <- MaybeT takeToken
      pure $ T pos
    f = runMaybeT $ do
      Leaf pos ('f' :| "alse") <- MaybeT takeToken
      pure $ F pos
    n = runMaybeT $ do
      Branch pos (Leaf _ ('n' :| "ot")) r <- MaybeT takeToken
      let r' = parse parseExpr r
      pure $ Not pos r'
    err = ExprErr <$> takeToken
  in t <<|>> f <<|>> n |>> err

treeTests :: Test
treeTests = tests
  (\s -> parse parseTokens (0, s))
  [("parseTree", parseTree)]
  [ ("", TreeErr Nothing)
  , (" ", TreeErr Nothing)
  , ("x", Leaf 0 ('x' :| []))
  , ("1", TreeErr (Just (TokenErr 0)))
  , (" x ", Leaf 1 ('x' :| []))
  , ("(f x)", Branch 0 (Leaf 1 ('f' :| [])) (Leaf 3 ('x' :| [])))
  , ("(f 1)", Branch 0 (Leaf 1 ('f' :| [])) (TreeErr (Just (TokenErr 3))))
  , (" ( f x ) ", Branch 1 (Leaf 3 ('f' :| [])) (Leaf 5 ('x' :| [])))
  ]

data Tree
  = Branch Int Tree Tree
  | Leaf Int (NonEmpty Char)
  | TreeErr (Maybe Token)
  deriving (Eq, Show)

instance Stream Tree where
  type T Tree = Tree
  token = Just

parseTree :: Parser [Token] Tree
parseTree =
  let
    leaf = runMaybeT $ do
      Name pos s <- MaybeT takeToken
      pure $ Leaf pos s
    branch = runMaybeT $ do
      Open pos <- MaybeT takeToken
      l <- lift parseTree
      r <- lift parseTree
      Close _ <- MaybeT takeToken
      pure $ Branch pos l r
    err = TreeErr <$> takeToken
  in leaf <<|>> branch |>> err

tokenTests :: Test
tokenTests = tests
  (0, )
  [("parseTokens", parseTokens), ("parseTokensM", parseTokensM)]
  [ ("", [])
  , ("()", [Open 0, Close 1])
  , ("(())", [Open 0, Open 1, Close 2, Close 3])
  , ("(a)", [Open 0, Name 1 ('a' :| []), Close 2])
  , ("foo()", [Name 0 ('f' :| "oo"), Open 3, Close 4])
  , ("(1)", [Open 0, TokenErr 1, Close 2])
  , (" ", [])
  , (" ( foo ) ", [Open 1, Name 3 ('f' :| "oo"), Close 7])
  , (" ( 12 ) ", [Open 1, TokenErr 3, TokenErr 4, Close 6])
  ]

data Token
  = Open Int
  | Close Int
  | Name Int (NonEmpty Char)
  | TokenErr Int
  deriving (Eq, Show)

parseTokens :: Parser (Int, String) [Token]
parseTokens =
  let
    skipSpace = void $ star $ try $ match ' '
    open = getPos >>= \pos -> try (match '(') $>> Open pos
    close = getPos >>= \pos -> try (match ')') $>> Close pos
    name = Name <$> getPos <*>> plus (try $ satisfy isAlpha)
    err = getPos >>= \pos -> TokenErr pos <<$ takeToken
    token = (open <<|>> close <<|>> name <<|>> err) <* skipSpace
    tokens = skipSpace *> star token
  in tokens

parseTokensM :: Parser (Int, String) [Token]
parseTokensM =
  let
    skipSpace = void $ star $ try $ match ' '
    open = runMaybeT $ do
      pos <- lift getPos
      MaybeT $ try $ match '('
      pure $ Open pos
    close = runMaybeT $ do
      pos <- lift getPos
      MaybeT $ try $ match ')'
      pure $ Close pos
    name = runMaybeT $ do
      pos <- lift getPos
      s <- MaybeT $ plus $ try $ satisfy isAlpha
      pure $ Name pos s
    err = runMaybeT $ do
      pos <- lift getPos
      MaybeT takeToken
      pure $ TokenErr pos
    token = do
      t <- open <<|>> close <<|>> name <<|>> err
      skipSpace
      pure t
    tokens = do
      skipSpace
      star token
  in tokens

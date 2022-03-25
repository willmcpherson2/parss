module Not (tokenTests, treeTests, exprTests) where

import Combinators
import Control.Arrow ((<<<), Arrow(arr))
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Char (isAlpha, isSpace)
import Data.List.NonEmpty (NonEmpty((:|)))
import Harness (tests)
import Parser
import Test.HUnit (Test)

exprTests :: Test
exprTests = tests
  (\s -> parse (parseTree <<< parseTokens) (0, 0, s))
  [("parseExpr", parseExpr)]
  [ ("", ExprErr (TreeErr Nothing))
  , ("true", T (0, 0))
  , ("false", F (0, 0))
  , ("(not true)", Not (0, 0) (T (0, 5)))
  , ("(not (not false))", Not (0, 0) (Not (0, 5) (F (0, 10))))
  , ("\n\ntrue\n\n", T (2, 0))
  ]

data Expr
  = T Pos
  | F Pos
  | Not Pos Expr
  | ExprErr Tree
  deriving (Eq, Show)

parseExpr :: Parser Tree Expr
parseExpr = arr $ \tree -> case tree of
  TreeErr{} -> ExprErr tree
  Leaf pos s -> case s of
    't' :| "rue" -> T pos
    'f' :| "alse" -> F pos
    _ -> ExprErr tree
  Branch pos l r -> case l of
    Leaf _ ('n' :| "ot") -> Not pos $ parse parseExpr r
    _ -> ExprErr tree

treeTests :: Test
treeTests = tests
  (\s -> parse parseTokens (0, 0, s))
  [("parseTree", parseTree)]
  [ ("", TreeErr Nothing)
  , (" ", TreeErr Nothing)
  , ("x", Leaf (0, 0) ('x' :| []))
  , ("1", TreeErr (Just (TokenErr (0, 0))))
  , (" x ", Leaf (0, 1) ('x' :| []))
  , ("(f x)", Branch (0, 0) (Leaf (0, 1) ('f' :| [])) (Leaf (0, 3) ('x' :| [])))
  , ( "(f 1)"
    , Branch (0, 0) (Leaf (0, 1) ('f' :| [])) (TreeErr (Just (TokenErr (0, 3))))
    )
  , ( " ( f x ) "
    , Branch (0, 1) (Leaf (0, 3) ('f' :| [])) (Leaf (0, 5) ('x' :| []))
    )
  ]

data Tree
  = Branch Pos Tree Tree
  | Leaf Pos (NonEmpty Char)
  | TreeErr (Maybe Token)
  deriving (Eq, Show)

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
  (0, 0, )
  [("parseTokens", parseTokens)]
  [ ("", [])
  , ("()", [Open (0, 0), Close (0, 1)])
  , ("(())", [Open (0, 0), Open (0, 1), Close (0, 2), Close (0, 3)])
  , ("(a)", [Open (0, 0), Name (0, 1) ('a' :| []), Close (0, 2)])
  , ("foo()", [Name (0, 0) ('f' :| "oo"), Open (0, 3), Close (0, 4)])
  , ("(1)", [Open (0, 0), TokenErr (0, 1), Close (0, 2)])
  , (" ", [])
  , (" ( foo ) ", [Open (0, 1), Name (0, 3) ('f' :| "oo"), Close (0, 7)])
  , (" ( 12 ) ", [Open (0, 1), TokenErr (0, 3), TokenErr (0, 4), Close (0, 6)])
  ]

data Token
  = Open Pos
  | Close Pos
  | Name Pos (NonEmpty Char)
  | TokenErr Pos
  deriving (Eq, Show)

type Pos = (Int, Int)

parseTokens :: Parser (Int, Int, String) [Token]
parseTokens =
  let
    skipSpace = void $ star $ try $ satisfyM isSpace
    open = runMaybeT $ do
      pos <- lift getPos
      MaybeT $ try $ matchM '('
      pure $ Open pos
    close = runMaybeT $ do
      pos <- lift getPos
      MaybeT $ try $ matchM ')'
      pure $ Close pos
    name = runMaybeT $ do
      pos <- lift getPos
      s <- MaybeT $ plus $ try $ satisfyM isAlpha
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

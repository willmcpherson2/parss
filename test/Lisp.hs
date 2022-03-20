{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lisp (exprTests, tokenTests, treeTests) where

import Combinators
import Control.Arrow ((<<<))
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Char (isAlpha, isSpace)
import Data.Either.Combinators (maybeToRight)
import Data.Either.Extra (fromEither)
import Data.List.NonEmpty (NonEmpty((:|)))
import Harness (tests)
import Parser
import Test.HUnit (Test)

exprTests :: Test
exprTests = tests
  (\s -> parse (parseTree <<< parseTokens) (0, s))
  [("parseExpr", parseExpr)]
  [ ("x", Var 0 ('x' :| ""))
  , ("(lambda (x y) x)", Fun 0 ['x' :| "", 'y' :| ""] (Var 14 ('x' :| "")))
  , ( "((lambda (x) x) y)"
    , App 0 [Fun 1 ['x' :| ""] (Var 13 ('x' :| "")), Var 16 ('y' :| "")]
    )
  , ("()", App 0 [])
  , ("(f)", App 0 [Var 1 ('f' :| "")])
  , ("(f x y)", App 0 [Var 1 ('f' :| ""), Var 3 ('x' :| ""), Var 5 ('y' :| "")])
  , ("1", ExprErr (UnexpectedChar 0 '1'))
  , ( "(lambda ((f x)) x)"
    , ExprErr
      (ParamErr
        (Branch
          0
          [ Leaf 1 ('l' :| "ambda")
          , Branch 8 [Branch 9 [Leaf 10 ('f' :| ""), Leaf 12 ('x' :| "")]]
          , Leaf 16 ('x' :| "")
          ]
        )
      )
    )
  , ( "(lambda (x))"
    , ExprErr $ FunErr $ Branch
      0
      [Leaf 1 ('l' :| "ambda"), Branch 8 [Leaf 9 ('x' :| "")]]
    )
  , ("(lambda)", ExprErr $ FunErr $ Branch 0 [Leaf 1 ('l' :| "ambda")])
  ]

data Expr
  = Fun Int [NonEmpty Char] Expr
  | App Int [Expr]
  | Var Int (NonEmpty Char)
  | ExprErr Err
  deriving (Show, Eq)

parseExpr :: Parser Tree Expr
parseExpr =
  let
    var = runMaybeT $ do
      Leaf pos s <- lift getStream
      pure $ Var pos s
    param = runMaybeT $ do
      Leaf _ s <- lift getStream
      pure s
    fun = runMaybeT $ do
      s <- lift getStream
      Branch pos (Leaf _ ('l' :| "ambda") : trees) <- pure s
      pure $ fromEither $ do
        (params, body) <- maybeToRight (ExprErr $ FunErr s) $ do
          [Branch _ params, body] <- pure trees
          pure (params, body)
        params' <- maybeToRight
          (ExprErr (ParamErr s))
          (mapM (parse param) params)
        let body' = parse parseExpr body
        pure $ Fun pos params' body'
    app = runMaybeT $ do
      Branch pos trees <- lift getStream
      let exprs = map (parse parseExpr) trees
      pure $ App pos exprs
    treeErr = runMaybeT $ do
      TreeErr err <- lift getStream
      pure $ ExprErr err
    err = ExprErr . SyntaxErr <$> getStream
  in try var <<|>> try fun <<|>> try app <<|>> try treeErr |>> err

--------------------------------------------------------------------------------

treeTests :: Test
treeTests = tests
  (\s -> parse parseTokens (0, s))
  [("parseTree", parseTree)]
  [ ("", TreeErr UnexpectedEof)
  , ("x", Leaf 0 ('x' :| ""))
  , ("x y", TreeErr (ExpectedEof (Name 2 ('y' :| ""))))
  , ("(f x)", Branch 0 [Leaf 1 ('f' :| ""), Leaf 3 ('x' :| "")])
  , ( "(f x y)"
    , Branch 0 [Leaf 1 ('f' :| ""), Leaf 3 ('x' :| ""), Leaf 5 ('y' :| "")]
    )
  , ( "((f x) (g y))"
    , Branch
      0
      [ Branch 1 [Leaf 2 ('f' :| ""), Leaf 4 ('x' :| "")]
      , Branch 7 [Leaf 8 ('g' :| ""), Leaf 10 ('y' :| "")]
      ]
    )
  ]

data Tree
  = Leaf Int (NonEmpty Char)
  | Branch Int [Tree]
  | TreeErr Err
  deriving (Show, Eq)

parseTree :: Parser [Token] Tree
parseTree =
  let
    leaf = runMaybeT $ do
      Name pos s <- MaybeT takeToken
      pure $ Leaf pos s
    branch = runMaybeT $ do
      Open pos <- MaybeT takeToken
      trees <- lift $ upto subTree $ try $ runMaybeT $ do
        Close{} <- MaybeT takeToken
        pure ()
      pure $ Branch pos trees
    tokenErr = runMaybeT $ do
      TokenErr err <- MaybeT takeToken
      pure $ TreeErr err
    err = runMaybeT $ do
      token <- MaybeT takeToken
      pure $ TreeErr (UnexpectedToken token)
    empty = pure $ TreeErr UnexpectedEof
    subTree =
      try leaf <<|>> try branch <<|>> try tokenErr <<|>> try err |>> empty
  in do
    t <- subTree
    takeToken >>= \case
      Just t -> pure $ TreeErr $ ExpectedEof t
      Nothing -> pure t

--------------------------------------------------------------------------------

tokenTests :: Test
tokenTests = tests
  (0, )
  [("parseTokens", parseTokens)]
  [ ("", [])
  , ("1", [TokenErr (UnexpectedChar 0 '1')])
  , ("(x)", [Open 0, Name 1 ('x' :| ""), Close 2])
  , ("(1)", [Open 0, TokenErr (UnexpectedChar 1 '1'), Close 2])
  , ( " ( foo bar ) "
    , [Open 1, Name 3 ('f' :| "oo"), Name 7 ('b' :| "ar"), Close 11]
    )
  ]

data Token
  = Open Int
  | Close Int
  | Name Int (NonEmpty Char)
  | TokenErr Err
  deriving (Show, Eq)

parseTokens :: Parser (Int, String) [Token]
parseTokens =
  let
    skipSpaces = void $ star $ try $ satisfyM isSpace
    open = runMaybeT $ do
      pos <- lift getPos
      MaybeT $ matchM '('
      pure $ Open pos
    close = runMaybeT $ do
      pos <- lift getPos
      MaybeT $ matchM ')'
      pure $ Close pos
    name = runMaybeT $ do
      pos <- lift getPos
      s <- MaybeT $ plus $ try $ satisfyM isAlpha
      pure $ Name pos s
    err = runMaybeT $ do
      pos <- lift getPos
      char <- MaybeT takeToken
      pure $ TokenErr (UnexpectedChar pos char)
    token = do
      skipSpaces
      try open <<|>> try close <<|>> try name <<|>> err
  in do
    tokens <- star token
    skipSpaces
    pure tokens

--------------------------------------------------------------------------------

data Err
  = UnexpectedChar Int Char
  | UnexpectedToken Token
  | UnexpectedEof
  | ExpectedEof Token
  | SyntaxErr Tree
  | FunErr Tree
  | ParamErr Tree
  | BodyErr Tree
  deriving (Show, Eq)

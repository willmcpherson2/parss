module Token (tokenTests) where

import Combinators
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Char (isAlpha)
import Data.List.NonEmpty (NonEmpty((:|)))
import Harness (tests)
import Parser
import Test.HUnit (Test)

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

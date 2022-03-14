module Parser (Parser(..)) where

import Control.Arrow
import Control.Category

newtype Parser s a = Parser { runParser :: s -> (s, a) }

instance Functor (Parser s) where
  fmap f (Parser p) = Parser $ \s -> let (s', x) = p s in (s', f x)

instance Applicative (Parser s) where
  pure x = Parser (, x)
  Parser p <*> Parser q = Parser $ \s ->
    let
      (s', f) = p s
      (s'', x) = q s'
    in (s'', f x)

instance Monad (Parser s) where
  Parser p >>= f = Parser $ \s ->
    let
      (s', x) = p s
      Parser q = f x
    in q s'

instance Category Parser where
  id = Parser $ \s -> (s, s)
  Parser p . Parser q = Parser $ \s ->
    let
      (_, x) = q s
      (_, y) = p x
    in (s, y)

instance Arrow Parser where
  arr f = Parser $ \s -> let x = f s in (s, x)
  first (Parser p) = Parser $ \(s, x) -> let (_, y) = p s in ((s, x), (y, x))

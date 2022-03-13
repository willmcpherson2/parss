module Parser (Parser(..)) where

newtype Parser s a = Parser (s -> (s, a))

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

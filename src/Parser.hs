{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser (Parser(..), Input, Result, Pos, Stream(..), runParser) where

import Prelude hiding (any)

newtype Parser s a = Parser (Input s -> Result s a)

type Input s = (Pos, s)

type Result s a = (Input s, a)

type Pos = Int

class Stream s where
  type T s
  stream :: Input s -> Either (Input s) (Result s (T s))

instance Stream [t] where
  type T [t] = t
  stream input@(pos, s) = case s of
    x : xs -> pure ((pos + 1, xs), x)
    [] -> Left input

instance Functor (Parser s) where
  fmap f (Parser p) = Parser (fmap f . p)

instance Applicative (Parser s) where
  pure x = Parser (, x)
  Parser p <*> Parser q =
    Parser $ \input -> let (input', f) = p input in f <$> q input'

instance Monad (Parser s) where
  Parser p >>= f =
    Parser $ \input -> let (input', x) = p input in f x `apply` input'

instance Semigroup a => Semigroup (Parser s a) where
  p <> q = (<>) <$> p <*> q

instance Monoid a => Monoid (Parser s a) where
  mempty = pure mempty

apply :: Parser s a -> Input s -> Result s a
apply (Parser p) = p

unwrap :: Result s a -> a
unwrap = snd

runParser :: Parser s a -> s -> a
runParser p s = unwrap $ apply p (0, s)

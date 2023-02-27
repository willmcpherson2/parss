module Parss.Parser (Parser (..), parse) where

import Control.Arrow (Arrow)
import qualified Control.Arrow
import Control.Category (Category)
import qualified Control.Category

newtype Parser m s a = Parser {runParser :: s -> (m, s, a)}

parse :: Parser m s a -> s -> a
parse p s = let (_, _, x) = runParser p s in x

instance Functor (Parser m s) where
  fmap f (Parser p) = Parser $ fmap f . p

instance Monoid m => Applicative (Parser m s) where
  pure x = Parser (mempty,,x)
  Parser p <*> Parser q = Parser $ \s ->
    let (m, s', f) = p s
        (m', s'', x) = q s'
     in (m <> m', s'', f x)

instance Monoid m => Monad (Parser m s) where
  Parser p >>= f = Parser $ \s ->
    let (m, s', x) = p s
        (m', s'', x') = runParser (f x) s'
     in (m <> m', s'', x')

instance Monoid m => Category (Parser m) where
  id = Parser $ \s -> (mempty, s, s)
  Parser p . Parser q = Parser $ \s ->
    let (m, _, x) = q s
        (m', _, y) = p x
     in (m <> m', s, y)

instance Monoid m => Arrow (Parser m) where
  arr f = Parser $ \s -> (mempty, s, f s)
  first (Parser p) = Parser $ \(s, x) ->
    let (m, _, y) = p s
     in (m, (s, x), (y, x))

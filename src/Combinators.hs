module Combinators
  ( untake,
    try,
    getToken,
    star,
    (<<|>>),
    match,
    satisfy,
    plus,
    (|>>),
    (!>>),
    (<<!),
    (<<|),
    upto,
    unless,
    into,
    intoM,
    matchM,
    rest,
    getPos,
    matchesM,
  )
where

import Control.Applicative (Alternative (empty), Applicative (liftA2))
import Control.Arrow (Arrow (arr))
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Parser
import Stream (Stream (takeToken))

untake :: Parser s a -> Parser s a
untake (Parser p) = Parser $ \s -> let (_, x) = p s in (s, x)

try :: Parser s (Maybe a) -> Parser s (Maybe a)
try (Parser p) = Parser $ \s ->
  let (s', x) = p s
   in case x of
        Just x -> (s', Just x)
        Nothing -> (s, Nothing)

getPos :: Parser (p, s) p
getPos = arr fst

getToken :: Stream s t => Parser s t
getToken = untake takeToken

rest :: (Applicative m, Semigroup (m a)) => Parser s a -> Parser s (m a)
rest p = liftA2 (<>) (pure <$> p) (rest p)

star :: Parser s (Maybe a) -> Parser s [a]
star p =
  p >>= \case
    Nothing -> pure []
    Just x -> (x :) <$> star p

plus :: Parser s (Maybe a) -> Parser s (Maybe (NonEmpty a))
plus = fmap nonEmpty . star

upto :: Parser s (Maybe a) -> Parser s (Maybe b) -> Parser s (Maybe [a])
upto p q = do
  xs <- star $ try $ p `unless` q
  y <- p
  pure $ y $> xs

unless :: Parser s (Maybe a) -> Parser s (Maybe b) -> Parser s (Maybe a)
unless p q =
  q >>= \case
    Just{} -> pure Nothing
    Nothing -> p

into :: Stream s t => (t -> a) -> Parser s a
into f = f <$> takeToken

intoM :: (Monad m, Stream s (m t)) => (t -> m a) -> Parser s (m a)
intoM f = into $ \x -> x >>= f

satisfy :: Stream s t => (t -> Bool) -> Parser s (Maybe t)
satisfy f = into $ \t -> if f t then Just t else Nothing

match :: (Stream s t, Eq t) => t -> Parser s (Maybe t)
match x = satisfy (== x)

matchM :: (Stream s t, Eq t) => t -> Parser s (Maybe t)
matchM x = satisfy (== x)

matchesM :: (Stream s a, Eq a) => [a] -> Parser s (Maybe [a])
matchesM = fmap sequence . sequence . map matchM

infixl 3 <<|>>

(<<|>>) :: Alternative f => Parser s (Maybe a) -> Parser s (Maybe a) -> Parser s (f a)
(Parser p) <<|>> (Parser q) = Parser $ \s -> case (p s, q s) of
  ((s', Just x), _) -> (s', pure x)
  (_, (s', Just x)) -> (s', pure x)
  _ -> (s, empty)

infixl 0 |>>

(|>>) :: Parser s (Maybe a) -> Parser s a -> Parser s a
p |>> q =
  p >>= \case
    Just x -> pure x
    Nothing -> q

infixl 0 <<|

(<<|) :: Parser s a -> Parser s (Maybe a) -> Parser s a
(<<|) = flip (|>>)

infixl 3 !>>

(!>>) :: Parser s (Maybe b) -> a -> Parser s (Either a b)
p !>> e =
  p <&> \case
    Just x -> Right x
    Nothing -> Left e

infixl 3 <<!

(<<!) :: a -> Parser s (Maybe b) -> Parser s (Either a b)
(<<!) = flip (!>>)

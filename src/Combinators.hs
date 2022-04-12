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
    satisfyM,
    matchM,
    getPos,
    matchesM,
    matches,
    endWith,
  )
where

import Control.Arrow (Arrow (arr))
import Data.Functor ((<&>))
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

into :: Stream s t => (t -> a) -> Parser s a
into f = f <$> takeToken

intoM :: Stream s (Maybe t) => (t -> Maybe a) -> Parser s (Maybe a)
intoM f = into $ \x -> x >>= f

satisfy :: Stream s a => (a -> Bool) -> Parser s (Maybe a)
satisfy f = into $ \t -> if f t then Just t else Nothing

satisfyM :: (Stream s (Maybe a)) => (a -> Bool) -> Parser s (Maybe a)
satisfyM f = intoM $ \t -> if f t then Just t else Nothing

match :: (Stream s t, Eq t) => t -> Parser s (Maybe t)
match x = satisfy (== x)

matchM :: (Stream s (Maybe a), Eq a) => a -> Parser s (Maybe a)
matchM x = satisfyM (== x)

matches :: (Stream s a, Eq a) => [a] -> Parser s (Maybe [a])
matches = fmap sequence . sequence . map match

matchesM :: (Stream s (Maybe a), Eq a) => [a] -> Parser s (Maybe [a])
matchesM = fmap sequence . sequence . map matchM

star :: Parser s (Maybe a) -> Parser s [a]
star p =
  p >>= \case
    Nothing -> pure []
    Just x -> (x :) <$> star p

plus :: Parser s (Maybe a) -> Parser s (Maybe (NonEmpty a))
plus = fmap nonEmpty . star

unless :: Parser s (Maybe a) -> Parser s (Maybe b) -> Parser s (Maybe a)
unless p q =
  q >>= \case
    Just{} -> pure Nothing
    Nothing -> p

upto :: Parser s (Maybe a) -> Parser s (Maybe b) -> Parser s [a]
upto p q = star $ p `unless` untake q

endWith :: Parser s (Maybe a) -> Parser s (Maybe b) -> Parser s (Maybe [a])
endWith p q = do
  xs <- p `upto` q
  x <- q
  pure $ xs <$ x

infixl 3 <<|>>

(<<|>>) :: Parser s (Maybe a) -> Parser s (Maybe a) -> Parser s (Maybe a)
(Parser p) <<|>> (Parser q) = Parser $ \s -> case (p s, q s) of
  ((s', Just x), _) -> (s', pure x)
  (_, (s', Just x)) -> (s', pure x)
  _ -> (s, Nothing)

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

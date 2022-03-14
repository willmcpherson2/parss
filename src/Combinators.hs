module Combinators
  ( untake
  , try
  , takeToken
  , getToken
  , takePos
  , getPos
  , rest
  , star
  , getTokenPos
  , (<<$>>)
  , (<<&>>)
  , getStream
  , (<<|>>)
  , match
  , satisfy
  , (<<$)
  , ($>>)
  , plus
  , (|>>)
  ) where

import qualified Control.Category as C
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Parser

getStream :: Parser s s
getStream = C.id

untake :: Parser s a -> Parser s a
untake (Parser p) = Parser $ \s -> let (_, x) = p s in (s, x)

try :: Parser s (Maybe a) -> Parser s (Maybe a)
try (Parser p) = Parser $ \s ->
  let (s', x) = p s
  in
    case x of
      Just x -> (s', Just x)
      Nothing -> (s, Nothing)

takeToken :: Parser [t] (Maybe t)
takeToken = Parser $ \s -> case s of
  [] -> (s, Nothing)
  t : ts -> (ts, Just t)

getToken :: Parser [t] (Maybe t)
getToken = untake takeToken

getTokenPos :: Parser [(Int, t)] (Maybe (Int, t))
getTokenPos = untake takeToken

takePos :: Parser [(Int, t)] (Maybe Int)
takePos = fmap fst <$> takeToken

getPos :: Parser [(Int, t)] (Maybe Int)
getPos = untake takePos

rest :: Parser s a -> Parser s [a]
rest p = (:) <$> p <*> rest p

star :: Parser s (Maybe a) -> Parser s [a]
star p = p >>= \case
  Nothing -> pure []
  Just x -> (x :) <$> star p

plus :: Parser s (Maybe a) -> Parser s (Maybe (NonEmpty a))
plus = fmap nonEmpty . star

satisfy :: (t -> Bool) -> Parser [t] (Maybe t)
satisfy f = takeToken <&> \case
  Just x -> if f x then Just x else Nothing
  Nothing -> Nothing

match :: Eq t => t -> Parser [t] (Maybe t)
match x = satisfy (== x)

infixl 3 <<|>>
(<<|>>) :: Parser s (Maybe a) -> Parser s (Maybe a) -> Parser s (Maybe a)
(Parser p) <<|>> (Parser q) = Parser $ \s -> case (p s, q s) of
  ((s', Just x), _) -> (s', Just x)
  (_, (s', Just x)) -> (s', Just x)
  _ -> (s, Nothing)

infixl 0 |>>
(|>>) :: Parser s (Maybe a) -> Parser s a -> Parser s a
p |>> q = p >>= \case
  Just x -> pure x
  Nothing -> q

infixl 4 <<$>>
(<<$>>) :: Functor f => (t -> a) -> Parser s (f t) -> Parser s (f a)
f <<$>> p = fmap f <$> p

infixl 4 <<$
(<<$) :: Functor f => a -> Parser s (f t) -> Parser s (f a)
x <<$ p = const x <<$>> p

infixl 4 $>>
($>>) :: Functor f => Parser s (f t) -> a -> Parser s (f a)
($>>) = flip (<<$)

infixl 1 <<&>>
(<<&>>) :: Functor f => Parser s (f t) -> (t -> a) -> Parser s (f a)
(<<&>>) = flip (<<$>>)

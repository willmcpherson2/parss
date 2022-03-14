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
  ) where

import Parser

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

infixl 1 <<$>>
(<<$>>) :: Functor f => (t -> a) -> Parser s (f t) -> Parser s (f a)
f <<$>> p = fmap f <$> p

infixl 1 <<&>>
(<<&>>) :: Functor f => Parser s (f t) -> (t -> a) -> Parser s (f a)
(<<&>>) = flip (<<$>>)

module Combinators
  ( untake
  , try
  , takeToken
  , getToken
  , takePos
  , getPos
  , rest
  , star
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

takePos :: Parser [(Int, t)] (Maybe Int)
takePos = Parser $ \s -> case s of
  [] -> (s, Nothing)
  (pos, _) : ts -> (ts, Just pos)

getPos :: Parser [(Int, t)] (Maybe Int)
getPos = untake takePos

rest :: Parser s a -> Parser s [a]
rest p = (:) <$> p <*> rest p

star :: Parser s (Maybe a) -> Parser s [a]
star p = p >>= \case
  Nothing -> pure []
  Just x -> (x :) <$> star p

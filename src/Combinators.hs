{-# LANGUAGE FlexibleContexts #-}

module Combinators
  ( untake
  , try
  , takeToken
  , getToken
  , takePos
  , getPos
  , star
  , getStream
  , (<<|>>)
  , match
  , satisfy
  , plus
  , (|>>)
  , (!>>)
  , eof
  , (<<!)
  , (<<|)
  , upto
  ) where

import Control.Applicative (Alternative(empty), Applicative(liftA2))
import qualified Control.Category as C
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Parser
import Stream

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

takeToken :: Stream s t => Parser s (Maybe t)
takeToken = toParser

getToken :: Stream s t => Parser s (Maybe t)
getToken = untake takeToken

takePos :: PosStream s t p => Parser s p
takePos = Parser $ \s -> let p = pos s in (update s, p)

getPos :: PosStream s t p => Parser s p
getPos = untake takePos

star :: Parser s (Maybe a) -> Parser s [a]
star p = p >>= \case
  Nothing -> pure []
  Just x -> (x :) <$> star p

plus :: Parser s (Maybe a) -> Parser s (Maybe (NonEmpty a))
plus = fmap nonEmpty . star

upto :: Parser s a -> Parser s (Maybe ()) -> Parser s [a]
upto p q = q >>= \case
  Just{} -> pure []
  Nothing -> liftA2 (:) p (upto p q)

satisfy :: (Stream s t, Alternative f) => (t -> Bool) -> Parser s (f t)
satisfy f = takeToken <&> \case
  Just x -> if f x then pure x else empty
  Nothing -> empty

match :: (Stream s t, Eq t, Alternative f) => t -> Parser s (f t)
match x = satisfy (== x)

eof :: Alternative f => Stream s t => Parser s (f ())
eof = getToken <&> \case
  Just _ -> empty
  Nothing -> pure ()

infixl 3 <<|>>
(<<|>>)
  :: Alternative f => Parser s (Maybe a) -> Parser s (Maybe a) -> Parser s (f a)
(Parser p) <<|>> (Parser q) = Parser $ \s -> case (p s, q s) of
  ((s', Just x), _) -> (s', pure x)
  (_, (s', Just x)) -> (s', pure x)
  _ -> (s, empty)

infixl 0 |>>
(|>>) :: Parser s (Maybe a) -> Parser s a -> Parser s a
p |>> q = p >>= \case
  Just x -> pure x
  Nothing -> q

infixl 0 <<|
(<<|) :: Parser s a -> Parser s (Maybe a) -> Parser s a
(<<|) = flip (|>>)

infixl 3 !>>
(!>>) :: Parser s (Maybe b) -> a -> Parser s (Either a b)
p !>> e = p <&> \case
  Just x -> Right x
  Nothing -> Left e

infixl 3 <<!
(<<!) :: a -> Parser s (Maybe b) -> Parser s (Either a b)
(<<!) = flip (!>>)

{-# LANGUAGE FlexibleContexts #-}

module Combinators
  ( untake
  , try
  , takeToken
  , getToken
  , takePos
  , getPos
  , star
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
  , (<<*>>)
  , (<*>>)
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

satisfy :: (Stream s t, Alternative f) => (t -> Bool) -> Parser s (f t)
satisfy f = takeToken <&> \case
  Just x -> if f x then pure x else empty
  Nothing -> empty

match :: (Stream s t, Eq t, Alternative f) => t -> Parser s (f t)
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
(<<$>>) :: (Functor f, Functor g) => (t -> a) -> f (g t) -> f (g a)
(<<$>>) = fmap . fmap

infixl 4 <<$
(<<$) :: (Functor f, Functor g) => a -> f (g t) -> f (g a)
x <<$ p = const x <<$>> p

infixl 4 $>>
($>>) :: (Functor f, Functor g) => f (g t) -> a -> f (g a)
($>>) = flip (<<$)

infixl 1 <<&>>
(<<&>>) :: (Functor f, Functor g) => f (g t) -> (t -> a) -> f (g a)
(<<&>>) = flip (<<$>>)

infixl 4 <<*>>
(<<*>>)
  :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

infixl 4 <*>>
(<*>>) :: (Applicative f, Applicative g) => f (a -> b) -> f (g a) -> f (g b)
x <*>> y = pure <$> x <<*>> y

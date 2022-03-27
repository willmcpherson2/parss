module Combinators
  ( untake
  , try
  , takeToken
  , getToken
  , getPos
  , star
  , getStream
  , (<<|>>)
  , match
  , satisfy
  , plus
  , (|>>)
  , (!>>)
  , (<<!)
  , (<<|)
  , upto
  , into
  , intoM
  , satisfyM
  , matchM
  , rest
  ) where

import Control.Applicative (Alternative(empty), Applicative(liftA2))
import Control.Arrow (Arrow(arr))
import qualified Control.Category as C
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Parser
import Stream (Stream(stream))
import GetPos (GetPos)
import qualified GetPos as G

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

takeToken :: Stream s t => Parser s t
takeToken = Parser stream

getToken :: Stream s t => Parser s t
getToken = untake takeToken

getPos :: GetPos s p => Parser s p
getPos = arr G.getPos

rest :: (Applicative m, Semigroup (m a)) => Parser s a -> Parser s (m a)
rest p = liftA2 (<>) (pure <$> p) (rest p)

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

into :: Stream s t => (t -> a) -> Parser s a
into f = f <$> takeToken

intoM :: (Monad m, Stream s (m t)) => (t -> m a) -> Parser s (m a)
intoM f = into $ \x -> x >>= f

satisfy :: (Stream s t, Alternative f) => (t -> Bool) -> Parser s (f t)
satisfy f = into $ \t -> if f t then pure t else empty

satisfyM
  :: (Monad m, Alternative m, Stream s (m a)) => (a -> Bool) -> Parser s (m a)
satisfyM f = intoM $ \t -> if f t then pure t else empty

match :: (Stream s t, Eq t, Alternative f) => t -> Parser s (f t)
match x = satisfy (== x)

matchM :: (Monad m, Alternative m, Stream s (m a), Eq a) => a -> Parser s (m a)
matchM x = satisfyM (== x)

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

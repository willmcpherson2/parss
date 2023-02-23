module Parss.Combinators
  ( source,
    match,
    left,
    right,
    locate,
    locateM,
    dry,
    try,
    into,
    satisfy,
    is,
    cat,
    literal,
    star,
    plus,
    or,
    orElse,
  )
where

import qualified Control.Category as C
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Parss.Parser
import Parss.Stream
import Prelude hiding (or)

source :: Monoid m => Parser m s s
source = C.id

match :: Parser m s a -> Parser m s (m, a)
match p = Parser $ \s ->
  let (m, s', x) = runParser p s
   in (m, s', (m, x))

left :: Monoid m => Parser m (l, r) l
left = Parser $ \(l, r) -> (mempty, (l, r), l)

right :: Monoid m => Parser m (l, r) r
right = Parser $ \(l, r) -> (mempty, (l, r), r)

locate :: Monoid m => Parser m (l, r) ((l, m, r) -> a) -> Parser m (l, r) a
locate p = do
  l <- left
  (m, f) <- match p
  r <- right
  pure $ f (l, m, r)

locateM ::
  (Monoid m, Applicative f) =>
  Parser m (l, r) (f ((l, m, r) -> a)) ->
  Parser m (l, r) (f a)
locateM = locate . fmap (\f x -> f <*> pure x)

dry :: Parser m s a -> Parser m s a
dry p = Parser $ \s ->
  let (m, _, x) = runParser p s
   in (m, s, x)

try :: Monoid m => Parser m s (Maybe a) -> Parser m s (Maybe a)
try p = Parser $ \s ->
  let (m, s', x) = runParser p s
   in case x of
        Just x -> (m, s', Just x)
        Nothing -> (mempty, s, Nothing)

into :: (Stream m s (n a), Monad n) => (a -> n b) -> Parser m s (n b)
into f = (>>= f) <$> stream

satisfy :: Stream m s (Maybe a) => (a -> Bool) -> Parser m s (Maybe a)
satisfy f = into $ \x -> if f x then Just x else Nothing

is :: (Stream m s (Maybe a), Eq a) => a -> Parser m s (Maybe a)
is x = satisfy (== x)

cat :: (Traversable t, Monad m, Monad n) => t (m (n a)) -> m (n (t a))
cat = fmap sequence . sequence

literal ::
  (Stream m s (Maybe a), Monoid m, Traversable t, Eq a) =>
  t a ->
  Parser m s (Maybe (t a))
literal = cat . fmap is

star :: Monad m => m (Maybe a) -> m [a]
star p =
  p >>= \case
    Just x -> (x :) <$> star p
    Nothing -> pure []

plus :: Monad m => m (Maybe a) -> m (Maybe (NonEmpty a))
plus = fmap nonEmpty . star

or ::
  Monoid m =>
  Parser m s (Maybe a) ->
  Parser m s (Maybe a) ->
  Parser m s (Maybe a)
or p q =
  try p >>= \case
    Just x -> pure $ pure x
    Nothing -> try q

infixl 3 `or`

orElse :: Monoid m => Parser m s (Maybe a) -> Parser m s a -> Parser m s a
orElse p q =
  try p >>= \case
    Just x -> pure x
    Nothing -> q

infixl 3 `orElse`

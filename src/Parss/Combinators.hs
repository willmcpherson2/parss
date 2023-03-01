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

-- | Get the source.
--
-- >>> parse (is 'a' >> source) ("", "abc")
-- ("a","bc")
source :: Monoid m => Parser m s s
source = C.id

-- | Matched input of parser, and its result.
--
-- >>> parse (match $ is 'a') "abc"
-- ("a",Just 'a')
match :: Parser m s a -> Parser m s (m, a)
match p = Parser $ \s ->
  let (m, s', x) = runParser p s
   in (m, s', (m, x))

-- | Left hand side of source, i.e. the previous input.
--
-- >>> parse (is 'a' >> left) ("", "abc")
-- "a"
left :: Monoid m => Parser m (l, r) l
left = Parser $ \(l, r) -> (mempty, (l, r), l)

-- | Right hand side of source, i.e. the remaining input.
--
-- >>> parse (is 'a' >> right) ("", "abc")
-- "bc"
right :: Monoid m => Parser m (l, r) r
right = Parser $ \(l, r) -> (mempty, (l, r), r)

-- | For a parser returning a result that accepts a location, creates
-- a parser that supplies the location. Keep in mind that the previous
-- input 'l' is usually in reverse because it's in order of
-- recency. This depends on the 'Stream' instance being used.
--
-- >>> parse (locate $ literal "34" >> pure (,True)) ("21", "3456")
-- (("21","34","56"),True)
locate :: Monoid m => Parser m (l, r) ((l, m, r) -> a) -> Parser m (l, r) a
locate p = do
  l <- left
  (m, f) <- match p
  r <- right
  pure $ f (l, m, r)

-- | 'locate' for a parser returning an applicative, usually @Maybe@.
locateM ::
  (Monoid m, Applicative f) =>
  Parser m (l, r) (f ((l, m, r) -> a)) ->
  Parser m (l, r) (f a)
locateM = locate . fmap (\f x -> f <*> pure x)

-- | Dry run of parser. Restores state.
--
-- >>> runParser (dry $ is 'a') "ax"
-- ("","ax",Just 'a')
--
-- >>> runParser (dry $ is 'a') "bx"
-- ("","bx",Nothing)
dry :: Monoid m => Parser m s a -> Parser m s a
dry p = Parser $ \s ->
  let (_, _, x) = runParser p s
   in (mempty, s, x)

-- | Try to run parser. Restore state on failure.
--
-- >>> runParser (try $ is 'a') "ax"
-- ("a","x",Just 'a')
--
-- >>> runParser (try $ is 'a') "bx"
-- ("","bx",Nothing)
try :: Monoid m => Parser m s (Maybe a) -> Parser m s (Maybe a)
try p = Parser $ \s ->
  let (m, s', x) = runParser p s
   in case x of
        Just x -> (m, s', Just x)
        Nothing -> (mempty, s, Nothing)

-- | Run stream into a function to create a parser.
--
-- >>> parse (into $ \x -> if x == 'a' then Just x else Nothing) "a"
-- Just 'a'
into :: (Stream m s (n a), Monad n) => (a -> n b) -> Parser m s (n b)
into f = (>>= f) <$> stream

-- | Create a parser for @Maybe a@ from a predicate @a -> Bool@.
--
-- >>> parse (satisfy (== 'a')) "a"
-- Just 'a'
satisfy :: Stream m s (Maybe a) => (a -> Bool) -> Parser m s (Maybe a)
satisfy f = into $ \x -> if f x then Just x else Nothing

-- | Like regular expression @a@.
--
-- >>> parse (is 'a') "a"
-- Just 'a'
is :: (Stream m s (Maybe a), Eq a) => a -> Parser m s (Maybe a)
is x = satisfy (== x)

-- | Like regular expression @ab@.
--
-- >>> parse (cat [is 'a', is 'b']) "ab"
-- Just "ab"
cat :: (Traversable t, Monad m, Monad n) => t (m (n a)) -> m (n (t a))
cat = fmap sequence . sequence

-- | Like regular expression @ab@.
--
-- >>> parse (literal "foo") "foo"
-- Just "foo"
literal ::
  (Stream m s (Maybe a), Monoid m, Traversable t, Eq a) =>
  t a ->
  Parser m s (Maybe (t a))
literal = cat . fmap is

-- | Like regular expression @a*@.
--
-- >>> parse (star $ try $ is 'a') "aaab"
-- "aaa"
star :: Monad m => m (Maybe a) -> m [a]
star p =
  p >>= \case
    Just x -> (x :) <$> star p
    Nothing -> pure []

-- | Like regular expression @a+@.
--
-- >>> parse (plus $ try $ is 'a') "aaab"
-- Just ('a' :| "aa")
plus :: Monad m => m (Maybe a) -> m (Maybe (NonEmpty a))
plus = fmap nonEmpty . star

-- | Like regular expression @a|b@.
--
-- >>> parse (try (is 'a') `or` is 'b') "a"
-- Just 'a'
or ::
  Monoid m =>
  Parser m s (Maybe a) ->
  Parser m s (Maybe a) ->
  Parser m s (Maybe a)
or p q =
  p >>= \case
    Just x -> pure $ pure x
    Nothing -> q

infixl 3 `or`

-- | Try @p@, otherwise fall back to @q@.
--
-- >>> parse (try (is 'a') `or` try (is 'b') `orElse` pure 'c') "x"
-- 'c'
orElse :: Monoid m => Parser m s (Maybe a) -> Parser m s a -> Parser m s a
orElse p q =
  p >>= \case
    Just x -> pure x
    Nothing -> q

infixl 3 `orElse`

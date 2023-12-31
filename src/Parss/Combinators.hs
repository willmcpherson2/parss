module Parss.Combinators
  ( source,
    match,
    parsed,
    remaining,
    locate,
    locateM,
    dry,
    try,
    into,
    satisfy,
    is,
    neg,
    end,
    liftResult,
    (><>),
    string,
    many,
    some,
    (<|>),
    (|>),
  )
where

import qualified Control.Category as C
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Parss.Parser
import Parss.Stream

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

-- | Left hand side of 'source', i.e. the input that has been
-- parsed. Usually the 'Stream' instance constructs the parsed input
-- via cons, so this might be in reverse.
--
-- >>> parse (is 'a' >> is 'b' >> parsed) ("", "abc")
-- "ba"
parsed :: Monoid m => Parser m (l, r) l
parsed = fst <$> source

-- | Right hand side of 'source', i.e. the remaining input.
--
-- >>> parse (is 'a' >> is 'b' >> remaining) ("", "abc")
-- "c"
remaining :: Monoid m => Parser m (l, r) r
remaining = snd <$> source

-- | For a parser with a result that takes a location, creates a
-- parser that supplies the location. Keep in mind that the previous
-- input 'l' is usually in reverse because it's in order of
-- recency. This depends on the 'Stream' instance being used.
--
-- >>> :{
-- parse34 = locate $ do
--   x <- string "34"
--   pure $ case x of
--     Just x -> (x,)
--     Nothing -> ("error",)
-- :}
--
-- >>> parse parse34 ("21", "3456")
-- ("34",("21","34","56"))
--
-- >>> parse parse34 ("21", "xx56")
-- ("error",("21","xx","56"))
locate :: Monoid m => Parser m (l, r) ((l, m, r) -> a) -> Parser m (l, r) a
locate p = do
  l <- parsed
  (m, f) <- match p
  r <- remaining
  pure $ f (l, m, r)

-- | 'locate' for a parser returning an applicative, usually @Maybe@.
--
-- >>> :{
-- parse34 = locateM $ do
--   x <- string "34"
--   pure $ case x of
--     Just x -> Just (x,)
--     Nothing -> Nothing
-- :}
--
-- >>> parse parse34 ("21", "3456")
-- Just ("34",("21","34","56"))
--
-- >>> parse parse34 ("21", "xx56")
-- Nothing
locateM ::
  (Monoid m, Applicative f) =>
  Parser m (l, r) (f ((l, m, r) -> a)) ->
  Parser m (l, r) (f a)
locateM = locate . fmap (\f x -> f <*> pure x)

-- | Dry run of parser. Restores state.
--
-- >>> runParser (dry $ is 'a') "ab"
-- ("","ab",Just 'a')
--
-- >>> runParser (dry $ is 'a') "xb"
-- ("","xb",Nothing)
dry :: Monoid m => Parser m s a -> Parser m s a
dry p = parse p <$> source

-- | Try to run parser. Restore state on failure.
--
-- >>> runParser (try $ is 'a') "ab"
-- ("a","b",Just 'a')
--
-- >>> runParser (try $ is 'a') "xb"
-- ("","xb",Nothing)
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

-- | Create a parser from a predicate @a -> Bool@.
--
-- >>> parse (satisfy (== 'a')) "a"
-- Just 'a'
satisfy :: Stream m s (Maybe a) => (a -> Bool) -> Parser m s (Maybe a)
satisfy f = into $ \x -> if f x then Just x else Nothing

-- | 'satisfy' of '=='. Like regular expression @a@.
--
-- >>> parse (is 'a') "a"
-- Just 'a'
is :: (Stream m s (Maybe a), Eq a) => a -> Parser m s (Maybe a)
is = satisfy . (==)

-- | Negate a parser. Maps the result to @Nothing@ if it succeeds and
-- @Just m@ if it fails.
-- Keep in mind that the resulting parser might match the end of the
-- stream.
--
-- >>> parse (neg $ string "x") "xxx"
-- Nothing
--
-- >>> parse (neg $ string "x") "abc"
-- Just "a"
--
-- >>> parse (many $ neg $ try (string "x") <|> end) "abc"
-- ["a","b","c"]
neg :: Parser m s (Maybe a) -> Parser m s (Maybe m)
neg p = Parser $ \s ->
  let (m, s', x) = runParser p s
   in case x of
        Just _ -> (m, s', Nothing)
        Nothing -> (m, s', Just m)

-- | End of stream. Like regular expression @$@.
--
-- >>> parse end "abc"
-- Nothing
--
-- >>> parse end ""
-- Just ""
end :: Stream m s (Maybe a) => Parser m s (Maybe m)
end = neg stream

-- | Lift the parser result into an applicative. For example, to
-- convert a parser of @Char@ to @String@.
--
-- >>> parse (liftResult $ is 'a') "a" :: Maybe String
-- Just "a"
liftResult :: (Functor f, Functor g, Applicative h) => f (g a) -> f (g (h a))
liftResult = fmap $ fmap pure

-- | Run @p@, then @q@, then combine the results. Kind of like a
-- combination of '>>' and '<>'. Like regular expression @ab@.
--
-- >>> parse (string "fish" ><> string "!") "fish!"
-- Just "fish!"
(><>) ::
  (Monad m, Monad n, Semigroup a) =>
  m (n a) ->
  m (n a) ->
  m (n a)
p ><> q = do
  x <- p
  y <- q
  pure $ do
    x' <- x
    y' <- y
    pure $ x' <> y'

infixr 6 ><>

-- | Match a sequence. Like regular expression @ab@.
--
-- >>> parse (string "ab") "abc"
-- Just "ab"
string ::
  (Monoid m, Traversable t, Stream m s (Maybe a), Eq a) =>
  t a ->
  Parser m s (Maybe (t a))
string = fmap sequence . traverse is

-- | Match zero or more. Like regular expression @a*@.
--
-- >>> parse (many $ try $ is 'a') "aaab"
-- "aaa"
many :: Monad m => m (Maybe a) -> m [a]
many p =
  p >>= \case
    Just x -> (x :) <$> many p
    Nothing -> pure []

-- | Match one or more. Like regular expression @a+@.
--
-- >>> parse (some $ try $ is 'a') "aaab"
-- Just ('a' :| "aa")
some :: Monad m => m (Maybe a) -> m (Maybe (NonEmpty a))
some = fmap nonEmpty . many

-- | Run @p@, if it fails, run @q@. Like regular expression @a|b@.
--
-- >>> parse (try (is 'a') <|> is 'b') "a"
-- Just 'a'
--
-- >>> parse (try (is 'a') <|> is 'b') "x"
-- Nothing
(<|>) ::
  Monoid m =>
  Parser m s (Maybe a) ->
  Parser m s (Maybe a) ->
  Parser m s (Maybe a)
p <|> q =
  p >>= \case
    Just x -> pure $ pure x
    Nothing -> q

infixr 1 <|>

-- | Run @p@, otherwise fall back to @q@.
--
-- >>> parse (try (is 'a') <|> is 'b' |> pure 'c') "a"
-- 'a'
--
-- >>> parse (try (is 'a') <|> is 'b' |> pure 'c') "x"
-- 'c'
(|>) :: Monoid m => Parser m s (Maybe a) -> Parser m s a -> Parser m s a
p |> q =
  p >>= \case
    Just x -> pure x
    Nothing -> q

infixr 0 |>

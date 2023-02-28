{-# LANGUAGE FunctionalDependencies #-}

module Parss.Stream (Stream (..)) where

import Parss.Parser (Parser (..))

-- | A 'Stream' is a 'Parser'. It is used at the bottom of the call
-- stack to actually get elements from the source.
--
-- 'Stream' instances such as @[a]@ are fine, but they only include
-- the remaining input. Instead, use instances like @([a], [a])@ which
-- tracks the previous input as well.
--
-- Because 'Parser's also track the matched input 'm', you end up with
-- a 3-tuple @([a], [a], [a])@, representing the previous, matched and
-- remaining input. With this, you have enough information to
-- construct an error message or calculate a numerical position in a
-- source file.
--
-- Keep in mind that "previous" means "most recent"! The list of
-- previous inputs is consed, so it will be the reverse of the
-- original source.
--
-- The "Parss.Combinators" module has helper functions for getting
-- different parts of the source.
class Stream m s a | s -> m a where
  stream :: Parser m s a

instance Stream [a] [a] (Maybe a) where
  stream = Parser $ \s -> case s of
    x : xs -> ([x], xs, Just x)
    [] -> ([], s, Nothing)

instance Stream [a] ([a], [a]) (Maybe a) where
  stream = Parser $ \(l, r) -> case r of
    x : xs -> ([x], (x : l, xs), Just x)
    [] -> ([], (l, r), Nothing)

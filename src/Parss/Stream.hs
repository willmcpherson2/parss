{-# LANGUAGE FunctionalDependencies #-}

module Parss.Stream (Stream (..)) where

import Parss.Parser (Parser (..))

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

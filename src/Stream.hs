{-# LANGUAGE FunctionalDependencies #-}

module Stream (Stream (..)) where

import Parser

class Stream s a | s -> a where
  takeToken :: Parser s a

instance Stream [a] (Maybe a) where
  takeToken = Parser $ \s -> case s of
    t : ts -> (ts, Just t)
    [] -> (s, Nothing)

instance Stream ([a], [a]) (Maybe a) where
  takeToken = Parser $ \(prev, next) -> case next of
    t : ts -> ((t : prev, ts), Just t)
    [] -> ((prev, []), Nothing)

{-# LANGUAGE FunctionalDependencies #-}

module Stream (Stream (..)) where

import Data.Text qualified as S
import Data.Text.Lazy qualified as L

class Stream s t | s -> t where
  stream :: s -> (s, t)

instance Stream [a] (Maybe a) where
  stream s = case s of
    t : ts -> (ts, Just t)
    [] -> (s, Nothing)

instance Stream S.Text (Maybe Char) where
  stream s = case S.uncons s of
    Just (t, ts) -> (ts, Just t)
    Nothing -> (s, Nothing)

instance Stream L.Text (Maybe Char) where
  stream s = case L.uncons s of
    Just (t, ts) -> (ts, Just t)
    Nothing -> (s, Nothing)

instance Enum p => Stream (p, [a]) (Maybe a) where
  stream s = case s of
    (pos, t : ts) -> ((succ pos, ts), Just t)
    _ -> (s, Nothing)

instance (Num l, Num c) => Stream (l, c, String) (Maybe Char) where
  stream s = case s of
    (line, _, t@'\n' : ts) -> ((line + 1, 0, ts), Just t)
    (line, column, t : ts) -> ((line, column + 1, ts), Just t)
    _ -> (s, Nothing)

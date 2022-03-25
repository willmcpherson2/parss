{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Stream (Stream(..), GetPos(..)) where

import qualified Data.Text as S
import qualified Data.Text.Lazy as L

class Stream s t | s -> t where
  stream :: s -> (s, t)

class GetPos s p | s -> p where
  getPos :: s -> p

instance Stream [a] (Maybe a) where
  stream = \case
    [] -> ([], Nothing)
    t : ts -> (ts, Just t)

instance Stream S.Text (Maybe Char) where
  stream = \s -> case S.uncons s of
    Nothing -> (s, Nothing)
    Just (t, ts) -> (ts, Just t)

instance Stream L.Text (Maybe Char) where
  stream = \s -> case L.uncons s of
    Nothing -> (s, Nothing)
    Just (t, ts) -> (ts, Just t)

instance Enum p => Stream (p, [a]) (Maybe a) where
  stream = \case
    (pos, []) -> ((pos, []), Nothing)
    (pos, t : ts) -> ((succ pos, ts), Just t)

instance GetPos (p, a) p where
  getPos = fst

instance (Num l, Num c) => Stream (l, c, String) (Maybe Char) where
  stream = \case
    (line, _, t@'\n' : ts) -> ((line + 1, 0, ts), Just t)
    (line, column, t : ts) -> ((line, column + 1, ts), Just t)
    s -> (s, Nothing)

instance GetPos (l, c, a) (l, c) where
  getPos = \(l, c, _) -> (l, c)

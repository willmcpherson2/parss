{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Stream (Stream(..), PosStream(..)) where

import qualified Control.Category as C
import Parser (Parser(Parser))

class Stream s t | s -> t where
  toParser :: Parser s t
  default toParser :: t ~ s => Parser s t
  toParser = C.id

class Stream s t => PosStream s t p | s -> p where
  pos :: s -> p
  default pos :: s ~ (p, a) => s -> p
  pos = fst

instance Stream [a] (Maybe a) where
  toParser = Parser $ \case
    [] -> ([], Nothing)
    t : ts -> (ts, Just t)

instance Enum p => Stream (p, [a]) (Maybe a) where
  toParser = Parser $ \case
    (pos, []) -> ((pos, []), Nothing)
    (pos, t : ts) -> ((succ pos, ts), Just t)

instance Enum p => PosStream (p, [a]) (Maybe a) p where

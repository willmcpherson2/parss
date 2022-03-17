{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

module Stream (Stream(..), PosStream(..)) where

import Data.Kind (Type)
import Parser (Parser(Parser))

class Stream s where
  type T s :: Type
  type T s = s

  token :: s -> Maybe (T s)
  default token :: T s ~ s => s -> Maybe (T s)
  token = Just

  state :: s -> s
  state = id

  update :: s -> s
  update = id

  toParser :: Parser s (Maybe (T s))
  toParser = Parser $ \s -> let t = token s in (update s, t)

class Stream s => PosStream s where
  type P s

  pos :: s -> P s

  toPosParser :: Parser s (P s)
  toPosParser = Parser $ \s -> let p = pos s in (update s, p)

instance Stream [t] where
  type T [t] = t

  token = \case
    [] -> Nothing
    t : _ -> Just t

  update = \case
    [] -> []
    _ : ts -> ts

instance Stream (Int, [t]) where
  type T (Int, [t]) = t

  token = token . snd

  update (pos, ts) = case ts of
    [] -> (pos, ts)
    _ : ts -> (pos + 1, ts)

instance PosStream (Int, [t]) where
  type P (Int, [t]) = Int

  pos = fst

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Stream (Stream(..), PosStream(..)) where

import Parser (Parser(Parser))

class Stream s t | s -> t where
  token :: s -> Maybe t
  default token :: t ~ s => s -> Maybe t
  token = Just

  state :: s -> s
  state = id

  update :: s -> s
  update = id

  toParser :: Parser s (Maybe t)
  toParser = Parser $ \s -> let t = token s in (update s, t)

class Stream s t => PosStream s t p | s -> p where
  pos :: s -> p
  default pos :: s ~ (p, a) => s -> p
  pos = fst

instance Stream [a] a where
  token = \case
    [] -> Nothing
    t : _ -> Just t
  update = \case
    [] -> []
    _ : ts -> ts

instance Enum p => Stream (p, [a]) a where
  token = token . snd
  update (pos, s) = case s of
    [] -> (pos, [])
    _ : ts -> (succ pos, ts)

instance Enum p => PosStream (p, [a]) a p where

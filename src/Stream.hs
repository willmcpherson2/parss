{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Stream (Stream(..), PosStream(..)) where

import Parser (Parser(Parser))

class Stream s t | s -> t where
  token :: s -> t
  default token :: t ~ s => s -> t
  token = id

  state :: s -> s
  state = id

  update :: s -> s
  update = id

  toParser :: Parser s t
  toParser = Parser $ \s -> let t = token s in (update s, t)

class Stream s t => PosStream s t p | s -> p where
  pos :: s -> p
  default pos :: s ~ (p, a) => s -> p
  pos = fst

instance Stream [a] (Maybe a) where
  token = \case
    [] -> Nothing
    t : _ -> Just t
  update = \case
    [] -> []
    _ : ts -> ts

instance Enum p => Stream (p, [a]) (Maybe a) where
  token = token . snd
  update (pos, s) = case s of
    [] -> (pos, [])
    _ : ts -> (succ pos, ts)

instance Enum p => PosStream (p, [a]) (Maybe a) p where

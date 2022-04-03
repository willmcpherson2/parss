{-# LANGUAGE FunctionalDependencies #-}

module Stream (Stream (..), Pos (..)) where

import Data.ByteString qualified as SB
import Data.ByteString.Lazy qualified as LB
import Data.List (uncons)
import Data.Text qualified as ST
import Data.Text.Lazy qualified as LT
import Data.Word (Word8)
import Parser

class Stream s a | s -> a where
  takeToken :: Parser s a

data Pos s = Pos {prev :: s, rest :: s}

doTakeToken :: (s -> Maybe (a, s)) -> (a -> s -> s) -> Parser (Pos s) (Maybe a)
doTakeToken uncons cons = Parser $ \s@Pos{prev, rest} -> case uncons rest of
  Just (t, rest) -> (Pos{prev = cons t prev, rest}, Just t)
  Nothing -> (s, Nothing)

instance Stream (Pos [a]) (Maybe a) where
  takeToken = doTakeToken uncons (:)

instance Stream (Pos ST.Text) (Maybe Char) where
  takeToken = doTakeToken ST.uncons ST.cons

instance Stream (Pos LT.Text) (Maybe Char) where
  takeToken = doTakeToken LT.uncons LT.cons

instance Stream (Pos SB.ByteString) (Maybe Word8) where
  takeToken = doTakeToken SB.uncons SB.cons

instance Stream (Pos LB.ByteString) (Maybe Word8) where
  takeToken = doTakeToken LB.uncons LB.cons

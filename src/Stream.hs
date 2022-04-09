{-# LANGUAGE FunctionalDependencies #-}

module Stream (Stream (..)) where

import Data.ByteString qualified as SB
import Data.ByteString.Lazy qualified as LB
import Data.List (uncons)
import Data.Text qualified as ST
import Data.Text.Lazy qualified as LT
import Data.Word (Word8)
import Parser

class Stream s a | s -> a where
  takeToken :: Parser s a

doTakeToken :: Enum p => (s -> Maybe (a, s)) -> Parser (p, s) (Maybe a)
doTakeToken uncons = Parser $ \(pos, s) -> case uncons s of
  Just (t, ts) -> ((succ pos, ts), Just t)
  Nothing -> ((pos, s), Nothing)

instance Enum p => Stream (p, [a]) (Maybe a) where
  takeToken = doTakeToken uncons

instance Enum p => Stream (p, ST.Text) (Maybe Char) where
  takeToken = doTakeToken ST.uncons

instance Enum p => Stream (p, LT.Text) (Maybe Char) where
  takeToken = doTakeToken LT.uncons

instance Enum p => Stream (p, SB.ByteString) (Maybe Word8) where
  takeToken = doTakeToken SB.uncons

instance Enum p => Stream (p, LB.ByteString) (Maybe Word8) where
  takeToken = doTakeToken LB.uncons

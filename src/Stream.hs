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

doTakePosToken :: Enum p => (s -> Maybe (a, s)) -> Parser (p, s) (Maybe a)
doTakePosToken uncons = Parser $ \(pos, s) -> case uncons s of
  Just (t, ts) -> ((succ pos, ts), Just t)
  Nothing -> ((pos, s), Nothing)

doTakeToken :: (s -> Maybe (a, s)) -> Parser s (Maybe a)
doTakeToken uncons = Parser $ \s -> case uncons s of
  Just (t, ts) -> (ts, Just t)
  Nothing -> (s, Nothing)

instance Stream [a] (Maybe a) where
  takeToken = doTakeToken uncons

instance Enum p => Stream (p, [a]) (Maybe a) where
  takeToken = doTakePosToken uncons

instance Enum p => Stream (p, ST.Text) (Maybe Char) where
  takeToken = doTakePosToken ST.uncons

instance Stream ST.Text (Maybe Char) where
  takeToken = doTakeToken ST.uncons

instance Enum p => Stream (p, LT.Text) (Maybe Char) where
  takeToken = doTakePosToken LT.uncons

instance Stream LT.Text (Maybe Char) where
  takeToken = doTakeToken LT.uncons

instance Enum p => Stream (p, SB.ByteString) (Maybe Word8) where
  takeToken = doTakePosToken SB.uncons

instance Stream SB.ByteString (Maybe Word8) where
  takeToken = doTakeToken SB.uncons

instance Enum p => Stream (p, LB.ByteString) (Maybe Word8) where
  takeToken = doTakePosToken LB.uncons

instance Stream LB.ByteString (Maybe Word8) where
  takeToken = doTakeToken LB.uncons

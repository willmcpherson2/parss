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

doTakeToken :: (s -> Maybe (a, s)) -> (a -> s -> s) -> Parser (s, s) (Maybe a)
doTakeToken uncons cons = Parser $ \s@(prev, rest) -> case uncons rest of
  Just (t, rest) -> ((cons t prev, rest), Just t)
  Nothing -> (s, Nothing)

instance Stream ([a], [a]) (Maybe a) where
  takeToken = doTakeToken uncons (:)

instance Stream (ST.Text, ST.Text) (Maybe Char) where
  takeToken = doTakeToken ST.uncons ST.cons

instance Stream (LT.Text, LT.Text) (Maybe Char) where
  takeToken = doTakeToken LT.uncons LT.cons

instance Stream (SB.ByteString, SB.ByteString) (Maybe Word8) where
  takeToken = doTakeToken SB.uncons SB.cons

instance Stream (LB.ByteString, LB.ByteString) (Maybe Word8) where
  takeToken = doTakeToken LB.uncons LB.cons

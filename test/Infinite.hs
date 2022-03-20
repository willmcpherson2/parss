{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Infinite (tokenTests) where

import Combinators
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Harness (tests)
import Parser
import Prelude hiding (repeat, take)
import Stream
import Test.HUnit (Test)

tokenTests :: Test
tokenTests = tests
  id
  [("parseTokens", take 10 <$> parseTokens)]
  [(repeat 'a', take 10 $ repeat A), (repeat 'c', take 10 $ repeat $ Err 'c')]

data Inf a = Cons a (Inf a)
  deriving (Show, Eq)

instance Stream (Inf a) a where
  toParser = Parser $ \(Cons x xs) -> (xs, x)

instance Functor Inf where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Inf where
  pure x = Cons x (pure x)
  Cons f _ <*> xs = fmap f xs

instance Semigroup (Inf a) where
  xs <> _ = xs

repeat :: a -> Inf a
repeat x = Cons x (repeat x)

take :: Int -> Inf a -> [a]
take n (Cons x xs)
  | n <= 0 = []
  | otherwise = x : take (n - 1) xs

data Token
  = A
  | B
  | Err Char
  deriving (Show, Eq)

parseTokens :: Parser (Inf Char) (Inf Token)
parseTokens =
  let
    a = runMaybeT $ do
      MaybeT $ match 'a'
      pure A
    b = runMaybeT $ do
      MaybeT $ match 'b'
      pure B
    err = Err <$> takeToken
    token = try a <<|>> try b |>> err
  in rest token

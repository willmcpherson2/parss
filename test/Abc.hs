module Abc (abcTests) where

import Combinators
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.Functor ((<&>))
import Harness (tests)
import Parser
import Test.HUnit (Test)

abcTests :: Test
abcTests = tests
  id
  [("parseAbc", parseAbc)]
  [ ("", Err ExpectedA)
  , ("x", Err ExpectedA)
  , ("a", Err ExpectedB)
  , ("ax", Err ExpectedB)
  , ("ab", Err ExpectedC)
  , ("abx", Err ExpectedC)
  , ("abc", Abc)
  , ("abcx", Err ExpectedEof)
  ]

data Abc
  = Abc
  | Err Err
  deriving (Show, Eq)

data Err
  = ExpectedA
  | ExpectedB
  | ExpectedC
  | ExpectedEof
  deriving (Show, Eq)

parseAbc :: Parser String Abc
parseAbc = parseErr <&> \case
  Left err -> Err err
  Right abc -> abc

parseErr :: Parser String (Either Err Abc)
parseErr = runExceptT $ do
  ExceptT $ match 'a' !>> ExpectedA
  ExceptT $ match 'b' !>> ExpectedB
  ExceptT $ match 'c' !>> ExpectedC
  ExceptT $ eof !>> ExpectedEof
  pure Abc

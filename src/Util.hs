module Util (parse, withPos) where

import Parser

parse :: Parser s a -> s -> a
parse (Parser p) = snd . p

withPos :: [a] -> [(Int, a)]
withPos = zip [0 ..]

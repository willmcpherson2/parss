{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module GetPos (GetPos(..)) where

class GetPos s p | s -> p where
  getPos :: s -> p

instance GetPos (p, a) p where
  getPos = fst

instance GetPos (l, c, a) (l, c) where
  getPos (l, c, _) = (l, c)

module Parss.Trans
  ( Fallible,
    fallible,
    Infallible,
    infallible,
    need,
    ok,
    fallback,
  )
where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Parss.Parser (Parser)

-- $setup
-- >>> import Parss.Combinators
-- >>> import Parss.Parser

-- | Parser with the power to return early with @Nothing@ and with
-- @Left e@.
type Fallible e m s = ExceptT e (MaybeT (Parser m s))

-- | Run a 'Fallible'.
fallible :: Fallible a m s a -> Parser m s (Maybe a)
fallible = fmap (fmap $ either id id) . runMaybeT . runExceptT

-- | Parser that always returns a value, but with the power to return
-- early with @Left e@.
type Infallible e m s = ExceptT e (IdentityT (Parser m s))

-- | Run an 'Infallible'.
infallible :: Infallible a m s a -> Parser m s a
infallible = fmap (either id id) . runIdentityT . runExceptT

-- | Parser @need@s a value to return @Just@.
--
-- >>> parse (fallible $ need (is 'a') >> need (is 'b') >> pure True) "ab"
-- Just True
--
-- >>> parse (fallible $ need (is 'a') >> need (is 'b') >> pure True) "ac"
-- Nothing
need :: (Monad m, MonadTrans t) => m (Maybe a) -> t (MaybeT m) a
need = lift . MaybeT

-- | Value is @ok@. Lifts it.
--
-- >>> parse (fallible $ ok (star $ try $ is 'a') >> need (is 'b')) "aaab"
-- Just 'b'
ok :: (Monad m, Monad (t m), MonadTrans t, MonadTrans u) => m a -> u (t m) a
ok = lift . lift

-- | Using default value 'e', return early if @Nothing@.
--
-- >>> parse (infallible $ fallback False (is 'a') >> pure True) "a"
-- True
--
-- >>> parse (infallible $ fallback False (is 'a') >> pure True) "b"
-- False
fallback :: (Monad m, MonadTrans t) => e -> m (Maybe a) -> ExceptT e (t m) a
fallback e = ExceptT . lift . fmap (maybe (Left e) Right)

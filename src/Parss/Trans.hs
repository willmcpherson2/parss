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

-- | Parser that always returns a value, but with the power to return
-- early with @Left e@.
type Infallible e m s = ExceptT e (IdentityT (Parser m s))

-- | Run an 'Infallible'.
infallible :: Infallible a m s a -> Parser m s a
infallible = fmap (either id id) . runIdentityT . runExceptT

-- | Parser with the power to return early with @Nothing@ and with
-- @Left e@.
type Fallible e m s = ExceptT e (MaybeT (Parser m s))

-- | Run a 'Fallible'.
fallible :: Fallible a m s a -> Parser m s (Maybe a)
fallible = fmap (fmap $ either id id) . runMaybeT . runExceptT

-- | Value is @ok@. Lifts it. Works for 'Fallible' and 'Infallible'.
--
-- >>> parse (fallible $ ok (star $ try $ is 'a') >> need (is 'b')) "aaab"
-- Just 'b'
ok ::
  (Monoid m, MonadTrans t, Monad (t (Parser m s))) =>
  Parser m s a ->
  ExceptT e (t (Parser m s)) a
ok = lift . lift

-- | Using default value 'e', return early if @Nothing@. Works for
-- 'Fallible' and 'Infallible'.
--
-- >>> parse (infallible $ fallback False (is 'a') >> pure True) "a"
-- True
--
-- >>> parse (infallible $ fallback False (is 'a') >> pure True) "b"
-- False
fallback ::
  (Monoid m, MonadTrans t) =>
  e ->
  Parser m s (Maybe a) ->
  ExceptT e (t (Parser m s)) a
fallback e = ExceptT . lift . fmap (maybe (Left e) Right)

-- | Parser @need@s a value to return @Just@.
--
-- >>> parse (fallible $ need (is 'a') >> need (is 'b') >> pure True) "ab"
-- Just True
--
-- >>> parse (fallible $ need (is 'a') >> need (is 'b') >> pure True) "ac"
-- Nothing
need :: Monoid m => Parser m s (Maybe a) -> Fallible e m s a
need = lift . MaybeT

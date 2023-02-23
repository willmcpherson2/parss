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

type Fallible e m s = ExceptT e (MaybeT (Parser m s))

fallible :: Fallible a m s a -> Parser m s (Maybe a)
fallible = fmap (fmap $ either id id) . runMaybeT . runExceptT

type Infallible e m s = ExceptT e (IdentityT (Parser m s))

infallible :: Infallible a m s a -> Parser m s a
infallible = fmap (either id id) . runIdentityT . runExceptT

need :: (Monad m, MonadTrans t) => m (Maybe a) -> t (MaybeT m) a
need = lift . MaybeT

ok :: (Monad m, Monad (t m), MonadTrans t, MonadTrans u) => m a -> u (t m) a
ok = lift . lift

fallback :: (Monad m, MonadTrans t) => e -> m (Maybe a) -> ExceptT e (t m) a
fallback e = ExceptT . lift . fmap (maybe (Left e) Right)

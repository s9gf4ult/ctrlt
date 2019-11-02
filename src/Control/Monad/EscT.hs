module Control.Monad.EscT where

import Control.Monad.CtrlT
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Cont

newtype EscT (e :: *) (s :: k) (r :: *) (m :: * -> *) (a :: *) = EscT
  { peelEscT :: (forall x. e -> CtrlT s r m x) -> CtrlT s r m a
  }

runEscT :: EscT e s r m a -> CtrlT s r m (Either e a)
runEscT (EscT ema) = ctrlForallCC $ \ (esc :: forall x. Either e a -> CtrlT s r m x) -> do
  Right <$> ema (esc . Left)

escape :: e -> EscT e s r m a
escape e = EscT $ \esc ->

eitherEscape :: Either e a -> EscT e s r m a
eitherEscape = \case
  Left e -> escape e
  Right a -> return a

instance Functor (EscT e s r m) where
  fmap f (EscT a) = EscT $ \esc -> fmap f (a esc)
  {-# INLINE fmap #-}

instance Applicative (EscT e s r m) where
  pure a = EscT $ \_esc -> pure a
  {-# INLINE pure #-}
  (EscT a) <*> (EscT b) = EscT $ \esc ->
    a esc <*> b esc
  {-# INLINE (<*>) #-}

instance Monad (EscT e s r m) where
  return = pure
  {-# INLINE return #-}
  (EscT ma) >>= f = EscT $ \esc -> do
    a <- ma esc
    peelEscT (f a) esc
  {-# INLINE (>>=) #-}

instance MonadTrans (EscT e s r) where
  lift ma = EscT $ \_esc -> lift ma
  {-# INLINE lift #-}

escCatch
  :: forall x m e t r a
  .  (MonadCatch m, Exception e)
  => (forall s. EscT x s a m a)
  -> (forall q. e -> EscT x q a m a)
  -> EscT x t r m a
escCatch ma handler = lift
  $ catch

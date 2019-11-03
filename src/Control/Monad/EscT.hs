module Control.Monad.EscT where

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.CtrlT
import           Control.Monad.CtrlT.Class
import           Control.Monad.Reader

newtype EscT (e :: *) (s :: k) (r :: *) (m :: * -> *) (a :: *) = EscT
  { peelEscT :: (forall x. e -> CtrlT s r m x) -> CtrlT s r m a
  }

runEscT :: EscT e s r m a -> CtrlT s r m (Either e a)
runEscT (EscT ema) = ctrlForallCC $ \ (esc :: forall x. Either e a -> CtrlT s r m x) -> do
  Right <$> ema (esc . Left)

evalEscT :: Monad m => EscT e s (Either e a) m a -> m (Either e a)
evalEscT ma = evalCtrlT $ runEscT ma
{-# INLINE evalEscT #-}

escape :: e -> EscT e s r m a
escape e = EscT $ \esc -> esc e

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

instance MonadCont (EscT e s r m) where
  callCC f = EscT $ \esc -> callCC $ \cc ->
    peelEscT (f $ \a -> EscT $ \_ -> cc a) esc
  {-# INLINE callCC #-}

instance (MonadCatch m) => IndexedMonadCatch (EscT e) m (Either e) where
  indexedCatch = escCatch
  {-# INLINE indexedCatch #-}

instance (Monad m) => IndexedMonadMask (EscT e) m (Either e) where
  indexedLiftMask = escLiftMask
  {-# INLINE indexedLiftMask #-}

escCatch
  :: forall x m e t r a
  .  (MonadCatch m, Exception e)
  => (forall s. EscT x s (Either x a) m a)
  -> (forall q. e -> EscT x q (Either x a) m a)
  -> EscT x t r m a
escCatch ma handler =
  (lift $ catch (evalEscT ma) (evalEscT . handler)) >>= eitherEscape

escLiftMask
  :: forall x r m t b
  .  (Monad m)
  => (forall d. ((forall a. m a -> m a) -> m d) -> m d)
  -> (forall s
      .  (forall a q
          .  EscT x q (Either x a) m a
          -> EscT x s (Either x b) m a)
      -> EscT x s (Either x b) m b)
  -> EscT x t r m b
escLiftMask mMask ma = do
  e <- lift $ mMask $ \restore -> do
    evalEscT $ ma $ \inner -> do
      e <- lift $ restore $ evalEscT inner
      eitherEscape e
  eitherEscape e
{-# INLINE escLiftMask #-}

escMask
  :: forall x r m t b
  .  (MonadMask m)
  => (forall s
      .  (forall a q
          .  EscT x q (Either x a) m a
          -> EscT x s (Either x b) m a)
      -> EscT x s (Either x b) m b)
  -> EscT x t r m b
escMask = escLiftMask mask
{-# INLINE escMask #-}

escUninterruptibleMask
  :: forall x r m t b
  .  (MonadMask m)
  => (forall s
      .  (forall a q
          .  EscT x q (Either x a) m a
          -> EscT x s (Either x b) m a)
      -> EscT x s (Either x b) m b)
  -> EscT x t r m b
escUninterruptibleMask = escLiftMask uninterruptibleMask
{-# INLINE escUninterruptibleMask #-}

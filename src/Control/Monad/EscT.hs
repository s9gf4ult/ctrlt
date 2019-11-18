module Control.Monad.EscT
  (EscT(peelEscT)
  , evalEscT
  , escape
  , eitherEscape
  , mapEscape
  ) where

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.CtrlT
import           Control.Monad.CtrlT.Class
import           Control.Monad.Except
import           Control.Monad.Reader

newtype EscT (e :: *) t (s :: k) (r :: *) (m :: * -> *) (a :: *) = EscT
  { peelEscT :: (forall x. e -> t s r m x) -> t s r m a
  }

evalEscT :: (ForallCC t, Functor (t s r m)) => EscT e t s r m a -> t s r m (Either e a)
evalEscT (EscT ema) = forallCC $ \ (esc :: forall x. Either e a -> t s r m x) -> do
  Right <$> ema (esc . Left)

escape :: e -> EscT e t s r m a
escape e = EscT $ \esc -> esc e

eitherEscape :: (Applicative (t s r m)) => Either e a -> EscT e t s r m a
eitherEscape = \case
  Left e  -> escape e
  Right a -> pure a

mapEscape
  :: (Monad (t s r m), ForallCC t)
  => (a -> b)
  -> EscT a t s r m x
  -> EscT b t s r m x
mapEscape f ma = EscT $ \esc -> do
  evalEscT ma >>= \case
    Right x -> return x
    Left a  -> esc (f a)

instance (Functor (t s r m)) => Functor (EscT e t s r m) where
  fmap f (EscT a) = EscT $ \esc -> fmap f (a esc)
  {-# INLINE fmap #-}

instance (Applicative (t s r m)) => Applicative (EscT e t s r m) where
  pure a = EscT $ \_esc -> pure a
  {-# INLINE pure #-}
  (EscT a) <*> (EscT b) = EscT $ \esc ->
    a esc <*> b esc
  {-# INLINE (<*>) #-}

instance (Monad (t s r m)) => Monad (EscT e t s r m) where
  return = pure
  {-# INLINE return #-}
  (EscT ma) >>= f = EscT $ \esc -> do
    a <- ma esc
    peelEscT (f a) esc
  {-# INLINE (>>=) #-}

instance (MonadTrans (t s r)) => MonadTrans (EscT e t s r) where
  lift ma = EscT $ \_esc -> lift ma
  {-# INLINE lift #-}

instance (MonadCont (t s r m)) => MonadCont (EscT e t s r m) where
  callCC f = EscT $ \esc -> callCC $ \cc ->
    peelEscT (f $ \a -> EscT $ \_ -> cc a) esc
  {-# INLINE callCC #-}

instance (ForallCC t) => ForallCC (EscT e t) where
  forallCC inner = EscT $ \esc -> forallCC $ \fcc ->
    peelEscT (inner $ \a -> EscT $ \_ -> fcc a) esc

instance (MonadThrow m, Monad (t s r m), MonadTrans (t s r))
  => MonadThrow (EscT e t s r m) where
  throwM e = lift $ throwM e
  {-# INLINE throwM #-}

instance (Monad m, Phoenix t m, ForallCC t, forall s r. Monad (t s r m))
  => Phoenix (EscT e t) m where
  type Dust (EscT e t) a = Dust t (Either e a)
  burnWith ma = EscT $ \esc -> do
    res <- burnWith $ \flame ->
      ma (\escT -> flame $ evalEscT escT)
    either esc return res
  reborn me = EscT $ \esc ->
    reborn me >>= either esc return

instance (Monad (t s r m), ForallCC t)
  => MonadError e (EscT e t s r m) where
  throwError = escape
  catchError ma handler = EscT $ \esc -> do
    evalEscT ma >>= \case
      Right a -> return a
      Left e  -> do
        evalEscT (handler e) >>= \case
          Right a -> return a
          Left  e -> esc e

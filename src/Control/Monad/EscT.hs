module Control.Monad.EscT
  (EscT(peelEscT)
  , evalEscT
  , runEscT
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

mapEscape :: (a -> b) -> EscT a s r m x -> EscT b s r m x
mapEscape f ma = EscT $ \esc -> do
  runEscT ma >>= \case
    Right x -> return x
    Left a  -> esc (f a)

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

instance (MonadThrow m) => MonadThrow (EscT e s r m) where
  throwM e = lift $ throwM e
  {-# INLINE throwM #-}

instance (Monad m) => Phoenix (EscT e) m where
  type Dust (EscT e) a = Either e a
  burnWith ma = reborn $ ma evalEscT
  reborn me = lift me >>= eitherEscape

instance MonadError e (EscT e s r m) where
  throwError = escape
  catchError ma handler = EscT $ \esc -> do
    runEscT ma >>= \case
      Right a -> return a
      Left e -> do
        runEscT (handler e) >>= \case
          Right a -> return a
          Left  e -> esc e

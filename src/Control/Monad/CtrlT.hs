module Control.Monad.CtrlT
  ( CtrlT(peelCtrlT)
  , evalCtrlT
  , runCtrlT
  ) where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.CtrlT.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Data.Coerce
import           Data.Functor.Identity

type role CtrlT nominal nominal representational representational

newtype CtrlT (s :: k) (r :: *) (m :: * -> *) (a :: *) = CtrlT
  { peelCtrlT :: ContT r m a
  } deriving (Functor, Applicative, Monad, MonadTrans, MonadCont)

instance (MonadBase b m) => MonadBase b (CtrlT s r m) where
  liftBase = lift . liftBase
  {-# INLINE liftBase #-}

instance (MonadThrow m) => MonadThrow (CtrlT s r m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

instance (Monad m) => Phoenix CtrlT m Identity where
  burnWith ma = lift $ runIdentity <$> (ma $ runCtrlT $ return . Identity)
  {-# INLINE burnWith #-}
  reborn mfa = lift $ runIdentity <$> mfa
  {-# INLINE reborn #-}

instance MonadContA CtrlT where
  callCCA inner = CtrlT $ fcc $ \esc ->
    peelCtrlT (inner $ CtrlT . esc)
    where
      fcc
        :: ((forall b. a -> ContT r m b) -> ContT r m a)
        -> ContT r m a
      fcc f = ContT $ \c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c

instance (Functor m) => MapResult CtrlT m  where
  mapResult f b (CtrlT (ContT xmaRe)) = CtrlT $ ContT $ \xmb ->
    fmap f $ xmaRe $ \x -> fmap b $ xmb x

evalCtrlT :: (Monad m) => CtrlT s a m a -> m a
evalCtrlT = runCtrlT return
{-# INLINE evalCtrlT #-}

runCtrlT :: (Monad m) => (a -> m r) -> CtrlT s r m a -> m r
runCtrlT ret (CtrlT cont) = runContT cont ret
{-# INLINE runCtrlT #-}

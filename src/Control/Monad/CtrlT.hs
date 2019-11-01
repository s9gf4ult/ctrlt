module Control.Monad.CtrlT where

import Control.Monad.Trans.Class
import Control.Monad.Cont
import Control.Monad.Trans.Cont
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.CtrlT.Class

type role CtrlT nominal nominal representational representational

newtype CtrlT (s :: k) r m a = CtrlT (ContT r m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

instance (MonadBase b m) => MonadBase b (CtrlT s r m) where
  liftBase = lift . liftBase
  {-# INLINE liftBase #-}

instance (MonadThrow m) => MonadThrow (CtrlT s r m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

instance (MonadCatch m) => IndexedMonadCatch m CtrlT where
  indCatch = ctrlCatch

instance (MonadMask m) => IndexedMonadMask m CtrlT where
  indMask = ctrlMask

evalCtrlT :: (Monad m) => CtrlT s a m a -> m a
evalCtrlT (CtrlT cont) = evalContT cont

ctrlCatch
  :: forall m e t r a
  .  (MonadCatch m, Exception e)
  => (forall s. CtrlT s a m a)
  -> (forall q. e -> CtrlT q a m a)
  -> CtrlT t r m a
ctrlCatch ma handler = lift
  $ catch (evalCtrlT ma) (evalCtrlT . handler)

ctrlMask
  :: forall r m t b
  .  (MonadMask m)
  => (forall s. (forall a q . CtrlT q a m a -> CtrlT s b m a) -> CtrlT s b m b)
  -> CtrlT t r m b
ctrlMask ma = lift $ mask $ \restore ->
  evalCtrlT (ma $ lift . restore . evalCtrlT)

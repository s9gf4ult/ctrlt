module Control.Monad.CtrlT where

import Control.Monad.Base
import Data.Coerce
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.CtrlT.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

type role CtrlT nominal nominal representational representational

newtype CtrlT (s :: k) (r :: *) (m :: * -> *) (a :: *) = CtrlT
  { peelCtrlT :: ContT r m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

deriving instance MonadCont (CtrlT s r m)

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
  indUninterruptibleMask = ctrlUninterruptibleMask

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

ctrlUninterruptibleMask
  :: forall r m t b
  .  (MonadMask m)
  => (forall s. (forall a q . CtrlT q a m a -> CtrlT s b m a) -> CtrlT s b m b)
  -> CtrlT t r m b
ctrlUninterruptibleMask ma = lift $ uninterruptibleMask $ \restore ->
  evalCtrlT (ma $ lift . restore . evalCtrlT)

forallCC
  :: ((forall b. a -> ContT r m b) -> ContT r m a)
  -> ContT r m a
forallCC f = ContT $ \ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c
{-# INLINE forallCC #-}

ctrlForallCC
  :: ((forall b. a -> CtrlT s r m b) -> CtrlT s r m a)
  -> CtrlT s r m a
ctrlForallCC inner = CtrlT $ forallCC $ \esc ->
  peelCtrlT (inner $ CtrlT . esc)
{-# INLINE ctrlForallCC #-}

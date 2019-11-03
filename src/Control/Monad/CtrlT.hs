module Control.Monad.CtrlT where

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

instance (MonadCatch m) => IndexedMonadCatch CtrlT m Identity where
  indexedCatch = ctrlCatch

instance (MonadMask m) => IndexedMonadMask CtrlT m Identity where
  indexedLiftMask = ctrlLiftMask

evalCtrlT :: (Monad m) => CtrlT s a m a -> m a
evalCtrlT = runCtrlT return

runCtrlT :: (Monad m) => (a -> m r) -> CtrlT s r m a -> m r
runCtrlT ret (CtrlT cont) = runContT cont ret

ctrlCatch
  :: forall m e t r a
  .  (MonadCatch m, Exception e)
  => (forall s. CtrlT s (Identity a) m a)
  -> (forall q. e -> CtrlT q (Identity a) m a)
  -> CtrlT t r m a
ctrlCatch ma handler = lift
  $ fmap runIdentity $ catch (runCtrlT (return . Identity) ma) (runCtrlT (return . Identity) . handler)

ctrlLiftMask
  :: forall r m t b
  .  (Monad m)
  => (forall d. ((forall a. m a -> m a) -> m d) -> m d)
  -> (forall s. (forall a q . CtrlT q (Identity a) m a -> CtrlT s (Identity b) m a) -> CtrlT s (Identity b) m b)
  -> CtrlT t r m b
ctrlLiftMask mMask ma = lift $ fmap runIdentity $ mMask $ \restore ->
  runCtrlT (return . Identity) (ma $ lift . fmap runIdentity . restore . runCtrlT (return . Identity))

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

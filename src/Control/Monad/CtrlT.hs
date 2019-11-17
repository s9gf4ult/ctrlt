module Control.Monad.CtrlT
  ( CtrlT(peelCtrlT)
  , evalCtrlT
  , runCtrlT
  , ctrlForallCC
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

instance (Monad m) => Phoenix CtrlT m where
  type Dust CtrlT a = a
  burnWith ma = lift $ ma evalCtrlT
  reborn = lift

evalCtrlT :: (Monad m) => CtrlT s a m a -> m a
evalCtrlT = runCtrlT return

runCtrlT :: (Monad m) => (a -> m r) -> CtrlT s r m a -> m r
runCtrlT ret (CtrlT cont) = runContT cont ret

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

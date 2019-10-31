module Control.Monad.CtrlT where

import Control.Monad.Trans.Class
import Control.Monad.Cont

type role CtrlT nominal nominal nominal representational

newtype CtrlT (s :: k) r m a = CtrlT (ContT r m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

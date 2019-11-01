module Control.Monad.CtrlT.Class where

import Control.Exception

class IndexedMonadCatch m (c :: k -> * -> (* -> *) -> * -> *) where
  indCatch
    :: forall e t r a
    .  (Exception e)
    => (forall s. c s a m a)
    -> (forall q. e -> c q a m a)
    -> c t r m a

class IndexedMonadMask m (c :: k -> * -> (* -> *) -> * -> *) where
  indMask
    :: forall r t b
    .  (forall s. (forall a q . c q a m a -> c s b m a) -> c s b m b)
    -> c t r m b

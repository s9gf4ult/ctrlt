module Control.Monad.CtrlT.Class where

import           Control.Exception

class IndexedMonadCatch (c :: k -> * -> (* -> *) -> * -> *) (m :: * -> *) (w :: * -> *) where
  indexedCatch
    :: forall e t r a
    .  (Exception e)
    => (forall s. c s (w a) m a)
    -> (forall q. e -> c q (w a) m a)
    -> c t r m a

class IndexedMonadMask (c :: k -> * -> (* -> *) -> * -> *) (m :: * -> *) (w :: * -> *)
  | c -> w where
  indexedLiftMask
    :: forall r t b
    .  (forall d. ((forall a. m a -> m a) -> m d) -> m d)
    -- ^ The 'mask' function from inner monad
    -> (forall s. (forall a q . c q (w a) m a -> c s (w b) m a) -> c s (w b) m b)
    -> c t r m b

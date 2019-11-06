module Control.Monad.CtrlT.Class where

import           Control.Exception

class IndexedMonadCatch (c :: k -> * -> (* -> *) -> * -> *) (m :: * -> *) (f :: * -> *) where
  indexedCatch
    :: forall e t r a
    .  (Exception e)
    => (forall s. c s (f a) m a)
    -> (forall q. e -> c q (f a) m a)
    -> c t r m a

class IndexedMonadMask (c :: k -> * -> (* -> *) -> * -> *) (m :: * -> *) (f :: * -> *)
  | c -> f where
  indexedLiftMask
    :: forall r t b
    .  (forall d. ((forall a. m a -> m a) -> m d) -> m d)
    -- ^ The 'mask' function from inner monad
    -> (forall s. (forall a q . c q (f a) m a -> c s (f b) m a) -> c s (f b) m b)
    -> c t r m b

class Phoenix (c :: k -> * -> (* -> *) -> * -> *) (m :: * -> *) where
  type Dust c a :: *
  burnout :: (forall s. c s (Dust c a) m a) -> m (Dust c a)
  reborn  :: Dust c a -> c s r m a

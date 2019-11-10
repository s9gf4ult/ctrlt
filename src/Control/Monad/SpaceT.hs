module Control.Monad.SpaceT where

import           Control.Monad.CtrlT.Class
import           Control.Monad.Trans.Class

newtype SpaceT t (d :: *) (s :: k) (r :: *) (m :: * -> *) (a :: *) = SpaceT
  { peelSpaceT :: d -> t s r m a
  } deriving Functor

instance (Applicative (t s r m)) => Applicative (SpaceT t d s r m) where
  pure a = SpaceT $ \_d -> pure a
  {-# INLINE pure #-}
  (SpaceT a) <*> (SpaceT b) = SpaceT $ \d ->
    a d <*> b d
  {-# INLINE (<*>) #-}

instance (Monad (t s r m)) => Monad (SpaceT t d s r m) where
  return = pure
  {-# INLINE return #-}
  (SpaceT a) >>= f = SpaceT $ \d -> do
    x <- a d
    peelSpaceT (f x) d
  {-# INLINE (>>=) #-}

instance (MonadTrans (t s r)) => MonadTrans (SpaceT t d s r) where
  lift ma = SpaceT $ \_d -> lift ma
  {-# INLINE lift #-}

instance (Phoenix t m) => Phoenix (SpaceT t d) m where
  type Dust (SpaceT t d) a = Dust t a
  burnWith ma = SpaceT $ \d -> burnWith $ \flame ->
    ma (\spaceT -> flame $ peelSpaceT spaceT d)
  reborn md = SpaceT $ \_d -> reborn md

module Control.Monad.SpaceT
  ( SpaceT(peelSpaceT)
  , evalSpaceT
  ) where

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.CtrlT.Class
import           Control.Monad.Except
import           Control.Monad.Trans.Class

newtype SpaceT (d :: *) t (s :: k) (r :: *) (m :: * -> *) (a :: *) = SpaceT
  { peelSpaceT :: d -> t s r m a
  } deriving Functor

evalSpaceT :: d -> SpaceT d t s r m a -> t s r m a
evalSpaceT d ma = peelSpaceT ma d
{-# INLINE evalSpaceT #-}

instance (Applicative (t s r m)) => Applicative (SpaceT d t s r m) where
  pure a = SpaceT $ \_d -> pure a
  {-# INLINE pure #-}
  (SpaceT a) <*> (SpaceT b) = SpaceT $ \d ->
    a d <*> b d
  {-# INLINE (<*>) #-}

instance (Monad (t s r m)) => Monad (SpaceT d t s r m) where
  return = pure
  {-# INLINE return #-}
  (SpaceT a) >>= f = SpaceT $ \d -> do
    x <- a d
    peelSpaceT (f x) d
  {-# INLINE (>>=) #-}

instance (MonadTrans (t s r)) => MonadTrans (SpaceT d t s r) where
  lift ma = SpaceT $ \_d -> lift ma
  {-# INLINE lift #-}

instance (MonadCont (t s r m)) => MonadCont (SpaceT d t s r m) where
  callCC f = SpaceT $ \d -> callCC $ \cc ->
    peelSpaceT (f $ \a -> SpaceT $ \_ -> cc a) d

instance (MonadThrow m, MonadTrans (t s r), Monad (t s r m))
  => MonadThrow (SpaceT d t s r m) where
  throwM e = lift $ throwM e
  {-# INLINE throwM #-}

instance (Phoenix t m f) => Phoenix (SpaceT d t) m f where
  burnWith ma = SpaceT $ \d -> burnWith $ \flame ->
    ma (\spaceT -> flame $ peelSpaceT spaceT d)
  reborn md = SpaceT $ \_d -> reborn md

instance (MonadContA t) => MonadContA (SpaceT d t) where
  callCCA inner = SpaceT $ \d -> callCCA $ \fcc ->
    peelSpaceT (inner $ \a -> SpaceT $ \_ -> fcc a) d
  {-# INLINE callCCA #-}

instance (MonadError e (t s r m)) => MonadError e (SpaceT d t s r m) where
  throwError e = SpaceT $ \_ -> throwError e
  {-# INLINE throwError #-}
  catchError ma handler = SpaceT $ \d ->
    catchError (peelSpaceT ma d) (\e -> peelSpaceT (handler e) d)
  {-# INLINE catchError #-}

module Control.Monad.CtrlT.Class where

import           Control.Monad.Catch

class Phoenix (c :: k -> * -> (* -> *) -> * -> *) (m :: * -> *) where
  type Dust c a :: *
  burnWith
    :: ((forall s b. c s (Dust c b) m b -> m (Dust c b)) -> m (Dust c a))
    -> c t r m a
  reborn  :: m (Dust c a) -> c s r m a

indexedCatch
    :: forall e c t r m a
    .  (Exception e, Phoenix c m, MonadCatch m)
    => (forall s. c s (Dust c a) m a)
    -> (forall q. e -> c q (Dust c a) m a)
    -> c t r m a
indexedCatch ma handler = burnWith $ \flame -> do
  catch (flame ma) (flame . handler)
{-# INLINE indexedCatch #-}

indexedMask
  :: forall c t r m b
  .  (Phoenix c m, MonadMask m)
  => (forall s
      .  (forall a q
          .  c q (Dust c a) m a
          -> c s (Dust c b) m a)
      -> c s (Dust c b) m b)
  -> c t r m b
indexedMask ma = burnWith $ \flame -> mask $ \restore ->
  flame (ma $ \restoring -> reborn $ restore $ flame restoring)
{-# INLINE indexedMask #-}

indexedUninterruptibleMask
  :: forall c t r m b
  .  (Phoenix c m, MonadMask m)
  => (forall s
      .  (forall a q
          .  c q (Dust c a) m a
          -> c s (Dust c b) m a)
      -> c s (Dust c b) m b)
  -> c t r m b
indexedUninterruptibleMask ma = burnWith $ \flame -> uninterruptibleMask $ \restore ->
  flame (ma $ \restoring -> reborn $ restore $ flame restoring)
{-# INLINE indexedUninterruptibleMask #-}

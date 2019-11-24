module Control.Monad.CtrlT.Class where

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Except

class Phoenix (c :: k -> * -> (* -> *) -> * -> *) (m :: * -> *) where
  type Dust c a :: *
  burnWith
    :: ((forall s b. c s (Dust c b) m b -> m (Dust c b)) -> m (Dust c a))
    -> c t r m a
  reborn  :: m (Dust c a) -> c s r m a

class MonadContA (c :: k -> * -> (* -> *) -> * -> *) where
  callCCA
    :: ((forall b. a -> c s r m b) -> c s r m a)
    -> c s r m a

type MonadThrowC c m = forall s r. MonadThrow (c s r m)

type MonadContC c m = forall s r. MonadCont (c s r m)

type MonadErrorC e c m = forall s r. MonadError e (c s r m)

type Eval c m a = forall s. c s (Dust c a) m a -> m a

indexedCatch
    :: forall e c t r m a
    .  (Exception e, Phoenix c m, MonadCatch m)
    => (forall s. c s (Dust c a) m a)
    -> (forall q. e -> c q (Dust c a) m a)
    -> c t r m a
indexedCatch ma handler = burnWith $ \flame -> do
  catch (flame ma) (flame . handler)
{-# INLINE indexedCatch #-}

data ErrorReason er ex
  = Err er
  | Exc ex

tryCatch :: (MonadError e m) => m a -> m (Either e a)
tryCatch ma = catchError (Right <$> ma) (return . Left)

-- indexedCatchError
--   :: forall c t r m a e x
--   .  (Phoenix c m, MonadCatch m, MonadErrorC e c m, Exception x)
--   => (forall s. c s (Dust c a) m a)
--   -> (forall q. ErrorReason e x -> c q (Dust c a) m a)
--   -> c t r m a
-- indexedCatchError ma handler = burnWith $ \flame -> mask $ \unmask -> do
--   res <- try $ unmask $ flame $ tryCatch ma
--   case res of
--     Left exc -> unmask $ flame $ handler $ Exc exc
--     Right dust ->

indexedCatchAll
  :: forall c t r m a
  .  (Phoenix c m, MonadCatch m)
  => (forall s. c s (Dust c a) m a)
  -> (forall q. SomeException -> c q (Dust c a) m a)
  -> c t r m a
indexedCatchAll = indexedCatch
{-# INLINE indexedCatchAll #-}

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

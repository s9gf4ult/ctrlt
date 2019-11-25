module Control.Monad.CtrlT.Class where

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Except

class (Applicative f, Traversable f)
  => Phoenix (c :: k -> * -> (* -> *) -> * -> *) (m :: * -> *) f
  |  c -> f where
  burnWith
    :: ((forall s b. c s (f b) m b -> m (f b)) -> m (f a))
    -> c t r m a
  reborn  :: m (f a) -> c s r m a

-- | Class of functors which may contain error, but not always do
class ErrorContainer e f | f -> e where
  splitError :: forall a. f a -> Maybe e

class MonadContA (c :: k -> * -> (* -> *) -> * -> *) where
  callCCA
    :: ((forall b. a -> c s r m b) -> c s r m a)
    -> c s r m a

class MapResult (c :: k -> * -> (* -> *) -> * -> *) m where
  mapResult :: (a -> b) -> (b -> a) -> c s a m x -> c s b m x

type MonadThrowC c m = forall s r. MonadThrow (c s r m)

type MonadContC c m = forall s r. MonadCont (c s r m)

type MonadErrorC e c m = forall s r. MonadError e (c s r m)

type Eval c m f a = forall s. c s (f a) m a -> m a

indexedCatch
    :: forall e c t r m a f
    .  (Exception e, Phoenix c m f, MonadCatch m)
    => (forall s. c s (f a) m a)
    -> (forall q. e -> c q (f a) m a)
    -> c t r m a
indexedCatch ma handler = burnWith $ \flame -> do
  catch (flame ma) (flame . handler)
{-# INLINE indexedCatch #-}

data ErrorReason er ex
  = Err er
  | Exc ex

tryCatch :: (MonadError e m) => m a -> m (Either e a)
tryCatch ma = catchError (Right <$> ma) (return . Left)

indexedCatchError
  :: forall c t r m a e x f
  .  (Phoenix c m f, MonadMask m, ErrorContainer e f, Exception x)
  => (forall s. c s (f a) m a)
  -> (forall q. ErrorReason e x -> c q (f a) m a)
  -> c t r m a
indexedCatchError ma handler = burnWith $ \flame -> mask $ \unmask -> do
  res <- try $ unmask $ flame ma
  case res of
    Left exc -> unmask $ flame $ handler $ Exc exc
    Right fa -> case splitError fa of
      Just err -> unmask $ flame $ handler $ Err err
      Nothing  -> return fa

-- indexedCatchAll
--   :: forall c t r m a
--   .  (Phoenix c m, MonadCatch m)
--   => (forall s. c s (Dust c a) m a)
--   -> (forall q. SomeException -> c q (Dust c a) m a)
--   -> c t r m a
-- indexedCatchAll = indexedCatch
-- {-# INLINE indexedCatchAll #-}

-- indexedMask
--   :: forall c t r m b
--   .  (Phoenix c m, MonadMask m)
--   => (forall s
--       .  (forall a q
--           .  c q (Dust c a) m a
--           -> c s (Dust c b) m a)
--       -> c s (Dust c b) m b)
--   -> c t r m b
-- indexedMask ma = burnWith $ \flame -> mask $ \restore ->
--   flame (ma $ \restoring -> reborn $ restore $ flame restoring)
-- {-# INLINE indexedMask #-}

-- indexedUninterruptibleMask
--   :: forall c t r m b
--   .  (Phoenix c m, MonadMask m)
--   => (forall s
--       .  (forall a q
--           .  c q (Dust c a) m a
--           -> c s (Dust c b) m a)
--       -> c s (Dust c b) m b)
--   -> c t r m b
-- indexedUninterruptibleMask ma = burnWith $ \flame -> uninterruptibleMask $ \restore ->
--   flame (ma $ \restoring -> reborn $ restore $ flame restoring)
-- {-# INLINE indexedUninterruptibleMask #-}

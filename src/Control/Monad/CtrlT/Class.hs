module Control.Monad.CtrlT.Class where

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Except
import           Data.Functor.Compose
import           Data.Functor.Identity

class (Applicative f, Traversable f)
  => Phoenix (c :: k -> * -> (* -> *) -> * -> *) (m :: * -> *) f
  |  c -> f where
  burnWith
    :: forall t r a
    .  ((forall b. (forall s i. c s i m b) -> m (f b)) -> m (f a))
    -> c t r m a
  reborn  :: forall s r a. m (f a) -> c s r m a

-- | Class of functors which may contain error, but not always do
class ErrorContainer e f where
  splitError :: forall a. f a -> Maybe e

instance ErrorContainer e Identity where
  splitError _ = Nothing

instance (Traversable f) => ErrorContainer e (Compose f (Either e)) where
  splitError (Compose fe) = case sequenceA fe of
    Left e -> Just e
    _      -> Nothing

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

data CatchReason er ex
  = Err er
  | Exc ex

tryCatch :: (MonadError e m) => m a -> m (Either e a)
tryCatch ma = catchError (Right <$> ma) (return . Left)

indexedCatch
  :: forall c t r m a e x f
  .  (Phoenix c m f, MonadMask m, ErrorContainer e f, Exception x)
  => (forall s r. c s r m a)
  -> (forall q r. CatchReason e x -> c q r m a)
  -> c t r m a
indexedCatch ma handler = burnWith $ \flame -> mask $ \unmask -> do
  res <- try $ unmask $ flame ma
  case res of
    Left exc -> unmask $ flame $ handler $ Exc exc
    Right fa -> case splitError fa of
      Just err -> unmask $ flame $ handler $ Err err
      Nothing  -> return fa

indexedCatchAll
  :: forall c t r m a e f
  .  (Phoenix c m f, MonadMask m, ErrorContainer e f)
  => (forall s r. c s r m a)
  -> (forall q r. CatchReason e SomeException -> c q r m a)
  -> c t r m a
indexedCatchAll = indexedCatch
{-# INLINE indexedCatchAll #-}

indexedMask
  :: forall c t r m b f
  .  (Phoenix c m f, MonadMask m)
  => (forall s i
      .  (forall a. (forall q j. c q j m a) -> c s i m a)
      -> c s i m b)
  -> c t r m b
indexedMask ma = burnWith $ \flame -> mask $ \restore ->
  flame (ma $ \restoring -> reborn $ restore $ flame restoring)
{-# INLINE indexedMask #-}

indexedUninterruptibleMask
  :: forall c t r m b f
  .  (Phoenix c m f, MonadMask m)
  => (forall s i
      .  (forall a. (forall q j. c q j m a) -> c s i m a)
      -> c s i m b)
  -> c t r m b
indexedUninterruptibleMask ma = burnWith $ \flame -> uninterruptibleMask $ \restore ->
  flame (ma $ \restoring -> reborn $ restore $ flame restoring)
{-# INLINE indexedUninterruptibleMask #-}

generalBracket
  :: forall c s t r m f x y z er ex
  .  ( Exception ex , Phoenix c m f, ErrorContainer er f
     , MonadMask m, MonadErrorC er c m )
  => (forall o r. c o r m x)
  -- ^ Acquire resource
  -> (forall q r. Either (CatchReason er ex) (x, y) -> c q r m z)
  -- ^ Free resource and/or do some cleanup
  -> (forall p r. x -> c p r m y)
  -- ^ Intermediate action
  -> c s r m (y, z)
generalBracket acquire release action = burnWith $ \flame -> mask $ \restore -> do
  res <- try $ restore $ flame $ do
    x <- acquire
    y <- action x
    return (x, y)
  case res of
    Left ex -> do
      void $ restore $ flame $ release $ Left $ Exc ex
      throwM ex
    Right fx -> case splitError fx of
      Just err -> restore $ flame $ do
        void $ release $ Left $ Err err
        throwError err
      Nothing -> restore $ flame $ do
        (x, y) <- reborn $ pure fx
        z <- release $ Right (x, y)
        return (y, z)

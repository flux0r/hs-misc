{-# LANGUAGE KindSignatures #-}

import Control.Applicative (Applicative, (<*>), (<$>), pure)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue,
                                      writeTQueue)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)

data Finalizer = MkFinalizer
    { finalize      :: !(STM ())
    , refCount      :: !(STM (TVar Int))
    }

newtype RegionT s m a = MkRegionT
    { unRegionT :: ReaderT (TQueue Finalizer) m a }

newtype FinalizerHandle (r :: * -> *) = FinalizerHandle Finalizer

mapRgn :: Monad m => (a -> b) -> RegionT s m a -> RegionT s m b
mapRgn f (MkRegionT fs) = MkRegionT (f `liftM` fs)

unitRgn :: Monad m => a -> RegionT s m a
unitRgn = MkRegionT . return

apRgn :: Monad m => RegionT s m (a -> b) -> RegionT s m a -> RegionT s m b
(MkRegionT fs) `apRgn` (MkRegionT m) = MkRegionT $ m >>= \x ->
    fs >>= \f -> return (f x)

bindRgn :: Monad m => RegionT s m a -> (a -> RegionT s m b) -> RegionT s m b
m `bindRgn` f = MkRegionT $ unRegionT m >>= unRegionT . f

instance Monad m => Functor (RegionT s m) where
    fmap = mapRgn

instance Monad m => Applicative (RegionT s m) where
    pure    = unitRgn
    (<*>)   = apRgn

instance Monad m => Monad (RegionT s m) where
    return  = unitRgn
    (>>=)   = bindRgn

onExit :: MonadIO m
       => STM ()
       -> RegionT s m (FinalizerHandle (RegionT s m))
onExit cleanup =
    MkRegionT $ ReaderT $ liftIO . atomically . register cleanup
  where
    register :: STM () -> TQueue Finalizer -> STM (FinalizerHandle r)
    register cleanup q = newTVar 1 >>= addFinalizer q . countRef cleanup

    countRef :: STM () -> TVar Int -> Finalizer
    countRef cleanup = MkFinalizer cleanup . return

    addFinalizer :: TQueue Finalizer -> Finalizer -> STM (FinalizerHandle r)
    addFinalizer q f = writeTQueue q f >> return (FinalizerHandle f)

before = newTQueue

doit r h = runReaderT (unRegionT r) h

after h = readTQueue h

f x =
    let finalizer   = finalize x
        cnt         = refCount x
    in  cnt >>= decrement

decrement x = atomically . modifyTVar x $ \cnt ->
    let cnt'    = cnt - 1
    in  cnt'

{-# LANGUAGE KindSignatures #-}

import Control.Applicative (Applicative, (<*>), (<$>), pure)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue,
                                      writeTQueue)
import Control.Monad (join, liftM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)

data Finalizer = MkFinalizer
    { finalize      :: !(IO ())
    , refCount      :: !(TVar Int)
    }

newtype RegionT s m a = MkRegionT
    { unRegionT :: ReaderT (TQueue Finalizer) m a }

newtype FinalizerHandle (r :: * -> *) = FinalizerHandle Finalizer

mapR :: Functor f => (a -> b) -> RegionT s f a -> RegionT s f b
mapR f (MkRegionT m) = MkRegionT (f <$> m)

unitR :: Monad f => a -> RegionT s f a
unitR x = MkRegionT (return x)

apR :: (Applicative f, Monad f)
    => RegionT s f (a -> b)
    -> RegionT s f a
    -> RegionT s f b
(MkRegionT ff) `apR` (MkRegionT fx) = MkRegionT $ ff <*> fx

bindR :: Monad f => RegionT s f a -> (a -> RegionT s f b) -> RegionT s f b
(MkRegionT m) `bindR` f = MkRegionT $ m >>= unRegionT . f

instance Functor f => Functor (RegionT s f) where
    fmap = mapR

instance (Applicative f, Monad f) => Applicative (RegionT s f) where
    pure    = unitR
    (<*>)   = apR

instance (Applicative f, Monad f) => Monad (RegionT s f) where
    return  = unitR
    (>>=)   = bindR

instance (Applicative f, MonadIO f) => MonadIO (RegionT s f) where
    liftIO = MkRegionT . liftIO

instance MonadTrans (RegionT s) where
    lift = MkRegionT . lift

--onExit cleanup = MkRegionT $ ReaderT $

f cleanup = newTVar 1 >>= return . MkFinalizer cleanup

{-# LANGUAGE KindSignatures #-}

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent.STM (STM, TQueue, TVar, newTVar, writeTQueue)
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)

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

--f :: IO () -> STM Finalizer
f cleanup = newTVar 1 >>= return . MkFinalizer cleanup

--g :: TQueue Finalizer -> Finalizer -> STM (FinalizerHandle r)
--g q h = h >>= \h' -> writeTQueue q h' >> return $ FinalizerHandle h

--h cleanup q = g q (f cleanup)

--onExit cleanup = MkRegionT $ ReaderT $ \q -> g q (f cleanup)

onExit cleanup = MkRegionT $ ReaderT $ \q -> return $
    let cleanup' = liftIO cleanup
    in  do
            cnt <- newTVar 1
            let h = MkFinalizer cleanup' cnt
            writeTQueue q h
            return $ FinalizerHandle h

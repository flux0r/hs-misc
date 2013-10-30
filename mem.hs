{-# LANGUAGE KindSignatures #-}

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent.STM (STM, TQueue, TVar, atomically, newTVar,
                               modifyTVar, writeTQueue)
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)

data Finalizer = MkFinalizer
    { finalize      :: !(IO ())
    , ref_cnt       :: !(TVar Int)
    }

newtype RegionT s m a = MkRegionT
    { un_region :: ReaderT (TQueue Finalizer) m a }

newtype FinalizerHandle (r :: * -> *) = FinalizerHandle Finalizer

map_region :: Functor f => (a -> b) -> RegionT s f a -> RegionT s f b
map_region f (MkRegionT m) = MkRegionT (f <$> m)

unit_region :: Monad f => a -> RegionT s f a
unit_region x = MkRegionT (return x)

ap_region :: (Applicative f, Monad f)
    => RegionT s f (a -> b)
    -> RegionT s f a
    -> RegionT s f b
(MkRegionT ff) `ap_region` (MkRegionT fx) = MkRegionT $ ff <*> fx

bind_region :: Monad f
            => RegionT s f a
            -> (a -> RegionT s f b)
            -> RegionT s f b
(MkRegionT m) `bind_region` f = MkRegionT $ m >>= un_region . f

instance Functor f => Functor (RegionT s f) where
    fmap = map_region

instance (Applicative f, Monad f) => Applicative (RegionT s f) where
    pure    = unit_region
    (<*>)   = ap_region

instance (Applicative f, Monad f) => Monad (RegionT s f) where
    return  = unit_region
    (>>=)   = bind_region

instance (Applicative f, MonadIO f) => MonadIO (RegionT s f) where
    liftIO = MkRegionT . liftIO

instance MonadTrans (RegionT s) where
    lift = MkRegionT . lift

class Duplicate h where
    duplicate :: (MonadIO f, Applicative f)
              => h (RegionT c (RegionT p f))
              -> RegionT c (RegionT p f) (h (RegionT p f))

instance Duplicate FinalizerHandle where
    duplicate = lift . copy

onExit :: MonadIO f => IO () -> RegionT s f (FinalizerHandle (RegionT s f))
onExit cleanup = MkRegionT $ ReaderT $ \q ->
    liftIO . atomically $ newTVar 1 >>= \cnt ->
    let h = MkFinalizer (liftIO cleanup) cnt
    in  writeTQueue q h >>
        return (FinalizerHandle h)

inc_ref_cnt :: TVar Int -> STM ()
inc_ref_cnt x = modifyTVar x (+ 1)

copy :: MonadIO f
     => FinalizerHandle a
     -> RegionT s f (FinalizerHandle (RegionT s f))
copy (FinalizerHandle h) =
    MkRegionT $ ReaderT $ \q ->
    (liftIO . atomically)
    ((inc_ref_cnt . ref_cnt) h >>
    writeTQueue q h >>
    return (FinalizerHandle h))

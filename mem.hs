import Control.Applicative (Applicative, (<*>), (<$>), pure)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TArray (TArray)
import Control.Monad (liftM)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

data Finalizer = MkFinalizer
    { finalize      :: !(IO ())
    , refCount      :: !(IO (TVar Int))
    }

newtype RegionT s m a = MkRegionT
    { unRegionT :: ReaderT (IO (TArray Int Finalizer)) m a }

mapRgn :: Monad m => (a -> b) -> RegionT s m a -> RegionT s m b
mapRgn f (MkRegionT fs) = MkRegionT (f `liftM` fs)

unitRgn :: Monad m => a -> RegionT s m a
unitRgn = MkRegionT . return

apRgn :: Monad m => RegionT s m (a -> b) -> RegionT s m a -> RegionT s m b
(MkRegionT fs) `apRgn` m = MkRegionT $ unRegionT m >>= \x ->
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

{-# LANGUAGE KindSignatures, RankNTypes, ScopedTypeVariables #-}

import Control.Applicative (Applicative ((<*>), pure), (<$>))
import Control.Concurrent.STM (STM, TBQueue, TVar, atomically, isEmptyTBQueue,
                               modifyTVar, newTBQueue, newTVar, readTBQueue,
                               readTVar, retry, writeTBQueue, writeTVar)
import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad (when, unless)
import Control.Monad.Reader (ReaderT (ReaderT), local, runReaderT)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (Ptr)

newtype ZoneT s p a = MkZoneT { un_zone_t :: ReaderT (TBQueue Ref) p a }

data Ref = MkRef
    { cleanup   :: IO ()
    , ref_cnt   :: TVar Int
    }

data Resource a r = MkResource a (Hnd r)

data Hnd (r :: * -> *) = MkHnd Ref

z_fmap :: Functor p => (a -> b) -> ZoneT s p a -> ZoneT s p b
f `z_fmap` (MkZoneT m) = MkZoneT (f <$> m)

z_pure :: Applicative p => a -> ZoneT s p a
z_pure = MkZoneT . pure

z_ap :: Applicative p => ZoneT s p (a -> b) -> ZoneT s p a -> ZoneT s p b
(MkZoneT f) `z_ap` (MkZoneT m) = MkZoneT (f <*> m)

z_return :: Monad p => a -> ZoneT s p a
z_return = MkZoneT . return

z_bind :: Monad p => ZoneT s p a -> (a -> ZoneT s p b) -> ZoneT s p b
(MkZoneT m) `z_bind` f = MkZoneT (m >>= un_zone_t . f)

instance Functor p => Functor (ZoneT s p) where
    fmap = z_fmap

instance Applicative p => Applicative (ZoneT s p) where
    pure    = z_pure
    (<*>)   = z_ap

instance Monad p => Monad (ZoneT s p) where
    return  = z_return
    (>>=)   = z_bind

new_ref :: IO () -> STM Ref
new_ref x = MkRef x <$> newTVar 1

insert_ref :: TBQueue Ref -> Ref -> STM (Hnd a)
insert_ref q ref = writeTBQueue q ref >> return (MkHnd ref)

liftAtomically :: MonadIO m => STM a -> m a
liftAtomically = liftIO . atomically

initialize :: IO () -> TBQueue Ref -> STM (Hnd a)
initialize c q = new_ref c >>= insert_ref q

on_exit :: MonadIO p => IO () -> ZoneT s p (Hnd (ZoneT s p))
on_exit c = MkZoneT . ReaderT $ liftAtomically . initialize c

inc_ref_cnt :: TVar Int -> STM ()
inc_ref_cnt = flip modifyTVar (+ 1)

dec_ref_cnt :: TVar Int -> STM ()
dec_ref_cnt = flip modifyTVar (subtract 1)

lift_zone_t :: Monad p => ZoneT s' p a -> ZoneT s (ZoneT s' p) a
lift_zone_t = MkZoneT . lift

class Monad m => ZoneIO m where
    bracket :: m a -> (a -> m b) -> (a -> m c) -> m c
    catch   :: Exception e => m a -> (e -> m a) -> m a
    lift_io :: IO a -> m a

reader_t_bracket :: ZoneIO m
                 => ReaderT r m a
                 -> (a -> ReaderT r m b)
                 -> (a -> ReaderT r m c)
                 -> ReaderT r m c
reader_t_bracket before after during = ReaderT $ \r ->
    let run = flip runReaderT r
    in  bracket (run before)
                (run . after)
                (run . during)

reader_t_catch :: (Exception e, ZoneIO m)
               => ReaderT r m a
               -> (e -> ReaderT r m a)
               -> ReaderT r m a
reader_t_catch m handler = ReaderT $ \r ->
    runReaderT m r `catch` \e -> runReaderT (handler e) r

reader_t_lift_io :: ZoneIO m => IO a -> ReaderT r m a
reader_t_lift_io = lift . lift_io

zone_t_bracket :: ZoneIO p
               => ZoneT s p a
               -> (a -> ZoneT s p b)
               -> (a -> ZoneT s p c)
               -> ZoneT s p c
zone_t_bracket before after during = MkZoneT $
    bracket (un_zone_t before)
            (un_zone_t . after)
            (un_zone_t . during)

zone_t_catch :: (Exception e, ZoneIO p)
             => ZoneT s p a
             -> (e -> ZoneT s p a)
             -> ZoneT s p a
zone_t_catch m handler = MkZoneT (un_zone_t m `catch` (un_zone_t . handler))

zone_t_lift_io :: ZoneIO p => IO a -> ZoneT s p a
zone_t_lift_io = MkZoneT . lift_io

instance ZoneIO IO where
    bracket = E.bracket
    catch   = E.catch
    lift_io = id

instance ZoneIO m => ZoneIO (ReaderT r m) where
    bracket = reader_t_bracket
    catch   = reader_t_catch
    lift_io = reader_t_lift_io

instance ZoneIO p => ZoneIO (ZoneT s p) where
    bracket = zone_t_bracket
    catch   = zone_t_catch
    lift_io = zone_t_lift_io

ref_runner :: Ref -> STM (IO ())
ref_runner (MkRef cleanup ref_cnt) = do
    dec_ref_cnt ref_cnt
    cnt <- readTVar ref_cnt
    if cnt == 0 then return cleanup else retry

queue_runner :: TBQueue Ref -> STM ()
queue_runner q = do
    is_empty <- isEmptyTBQueue q
    unless is_empty $ do
        x <- readTBQueue q
        ref_runner x
        queue_runner q

runZoneT :: ZoneIO p => (forall s. ZoneT s p a) -> Int -> p a
runZoneT z n = 
    let before      = lift_io $ atomically (newTBQueue n)
        after       = lift_io . atomically . queue_runner
        during q    = runReaderT (un_zone_t z) q
    in bracket before after during

newZonePtr :: Ptr a -> STM (Resource (Ptr a) r)
newZonePtr p = new_ref (free p) >>= return . MkResource p . MkHnd

wrapMallocBytes :: ZoneIO m => Int -> m (Resource (Ptr a) r)
wrapMallocBytes n = lift_io (mallocBytes n >>= atomically . newZonePtr)

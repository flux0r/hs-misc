{-# LANGUAGE KindSignatures, RankNTypes #-}

import Control.Applicative (Applicative ((<*>), pure), (<$>))
import Control.Concurrent.STM (STM, TBQueue, TVar, atomically, modifyTVar,
                               newTVar, readTVar, writeTBQueue, writeTVar)
import Control.Exception (assert)
import Control.Monad.Reader (ReaderT (ReaderT), local)
import Control.Monad.Trans (MonadIO, lift, liftIO)

newtype ZoneT s p a = MkZoneT { un_zone_t :: ReaderT (TBQueue Ref) p a }

data Ref = MkRef
    { cleanup   :: IO ()
    , ref_cnt   :: TVar Int
    }

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
inc_ref_cnt = flip modifyTVar (subtract 1)

dec_ref_cnt :: TVar Int -> STM ()
dec_ref_cnt = flip modifyTVar (+ 1)

lift_zone_t :: Monad p => ZoneT s' p a -> ZoneT s (ZoneT s' p) a
lift_zone_t = MkZoneT . lift

{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative (Applicative ((<*>), pure),  (<$>))
import Control.Exception (Exception)
import Control.Monad (join)
import Control.Monad.Reader (ReaderT (ReaderT), runReaderT)
import Control.Monad.Trans (lift)
import Control.Concurrent.STM (STM, TBQueue, TVar, atomically, modifyTVar,
                               newTBQueue, newTVar, readTVar, retry)

import qualified Control.Exception as E

newtype ZoneT s r p a = MkZoneT { un_zone_t :: ReaderT (TBQueue r) p a }

data Hnd = MkHnd (IO ()) (TVar Int)

finalizer :: Hnd -> IO ()
finalizer (MkHnd x _) = x

ref_cnt :: Hnd -> TVar Int
ref_cnt (MkHnd _ x) = x

data Resource r = MkResource r Hnd

resource :: Resource r -> r
resource (MkResource x _) = x

hnd :: Resource r -> Hnd
hnd (MkResource _ x) = x

close_resource :: Resource r -> IO ()
close_resource (MkResource x h) = join . atomically $ do
    let rc      = ref_cnt h
        cleanup = finalizer h
    cnt <- readTVar rc
    modifyTVar rc (subtract 1)
    if (cnt > 1) then retry else (return cleanup)

new_resource :: r -> IO () -> STM (Resource r)
new_resource x cleanup = newTVar 1 >>= return . MkResource x . MkHnd cleanup

fmap_z :: Functor p => (a -> b) -> ZoneT s r p a -> ZoneT s r p b
fmap_z f (MkZoneT r) = MkZoneT (f <$> r)

pure_z :: Applicative p => a -> ZoneT s r p a
pure_z x = MkZoneT (pure x)

ap_z :: Applicative p
     => ZoneT s r p (a -> b)
     -> ZoneT s r p a
     -> ZoneT s r p b
(MkZoneT f) `ap_z` (MkZoneT r) = MkZoneT (f <*> r)

return_z :: Monad p => a -> ZoneT s r p a
return_z x = MkZoneT (return x)

bind_z :: Monad p => ZoneT s r p a -> (a -> ZoneT s r p b) -> ZoneT s r p b
(MkZoneT r) `bind_z` f = MkZoneT (r >>= un_zone_t . f)

instance Functor p => Functor (ZoneT s r p) where
    fmap = fmap_z

instance Applicative p => Applicative (ZoneT s r p) where
    pure    = pure_z
    (<*>)   = ap_z

instance Monad p => Monad (ZoneT s r p) where
    return  = return_z
    (>>=)   = bind_z

lift_zone :: Monad p => ZoneT s r p a -> ZoneT s' r' (ZoneT s r p) a
lift_zone = MkZoneT . lift

class (Monad p, Monad p') => ZoneLift p p' where
    lifts :: p a -> p' a

class Monad p => ZoneIO p where
    bracket :: p a -> (a -> p b) -> (a -> p c) -> p c
    catch   :: Exception e => p a -> (e -> p a) -> p a
    lift_io :: IO a -> p a

instance ZoneIO IO where
    bracket = E.bracket
    catch   = E.catch
    lift_io = id

bracket_r :: ZoneIO p
          => ReaderT r p a
          -> (a -> ReaderT r p b)
          -> (a -> ReaderT r p c)
          -> ReaderT r p c
bracket_r before after during = ReaderT r
  where
    r x = let runner p = runReaderT p x in
        bracket (runner before)
                (runner . after)
                (runner . during)

catch_r :: (Exception e, ZoneIO p)
        => ReaderT r p a
        -> (e -> ReaderT r p a)
        -> ReaderT r p a
catch_r x handler = ReaderT $ \r ->
    catch (runReaderT x r)
          (\e -> runReaderT (handler e) r)

lift_io_r :: ZoneIO p => IO a -> ReaderT r p a
lift_io_r = lift . lift_io

instance ZoneIO p => ZoneIO (ReaderT r p) where
    bracket = bracket_r
    catch   = catch_r
    lift_io = lift_io_r

bracket_z :: ZoneIO p
          => ZoneT s r p a
          -> (a -> ZoneT s r p b)
          -> (a -> ZoneT s r p c)
          -> ZoneT s r p c
bracket_z before after during = MkZoneT $
    bracket (un_zone_t before)
            (un_zone_t . after)
            (un_zone_t . during)

catch_z :: (Exception e, ZoneIO p)
        => ZoneT s r p a
        -> (e -> ZoneT s r p a)
        -> ZoneT s r p a
catch_z x handler = MkZoneT (catch (un_zone_t x)
                                   (un_zone_t . handler))
lift_io_z :: ZoneIO p => IO a -> ZoneT s r p a
lift_io_z = MkZoneT . lift_io

instance ZoneIO p => ZoneIO (ZoneT s r p) where
    bracket = bracket_z
    catch   = catch_z
    lift_io = lift_io_z

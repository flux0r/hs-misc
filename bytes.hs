{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}

------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Control.Category (Category ((.), id))
import Control.Monad (Monad ((>>), (>>=), return))
import Data.Bits (shiftR)
import Data.Bool (Bool (True, False), (&&), otherwise)
import Data.Eq (Eq ((/=), (==)))
import Data.Function (($))
import Data.Int (Int)
import Data.Ord (Ord, Ordering (EQ, GT, LT), (>), (<), compare, min)
import Data.Word (Word64)
import Foreign.C.Types (CInt (CInt), CSize (CSize))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (poke)
import GHC.Base (realWorld#)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (IO (IO), unsafeDupablePerformIO)
import GHC.Prim (Addr#)
import GHC.Ptr (Ptr (Ptr))
import Prelude (fromIntegral)
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- Standard C functions                                                     --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | int memcmp(const void *s1, const void *s2, size_t n);

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word64 -> Ptr Word64 -> CSize -> IO CInt

memcmp :: Ptr Word64 -> Ptr Word64 -> Int -> IO CInt
memcmp p q = c_memcmp p q . fromIntegral



------------------------------------------------------------------------------
-- Data representations                                                     --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A Word64 vector.
--
-- A Buffer is a Ptr Word64 payload, an Int offset in Word64s, and an Int --
-- length in octets.

data Buffer = B (Ptr Word64) Int Int



------------------------------------------------------------------------------
-- | Http types

data HttpVersion = MkHttpVersion Int Int

instance (Eq HttpVersion) where
    (MkHttpVersion l0 r0) == (MkHttpVersion l1 r1) =
        l0 == l1 && r0 == r1

instance (Ord HttpVersion) where
    compare (MkHttpVersion l0 r0) (MkHttpVersion l1 r1)
        | l0 < l1       = LT
        | l0 > l1       = GT
        | otherwise     = compare r0 r1


------------------------------------------------------------------------------
-- Low-level construction and destruction                                   --
------------------------------------------------------------------------------


------------------------------------------------------------------------------

mk_buffer :: Ptr Word64 -> Int -> Int -> Buffer
mk_buffer = B


------------------------------------------------------------------------------

un_buffer :: Buffer -> (Ptr Word64, Int, Int)
un_buffer (B p i l) = (p, i, l)


------------------------------------------------------------------------------
-- | Create a Buffer of size l using Ptr p and then fill it with action f.

create :: Ptr Word64 -> Int -> (Ptr Word64 -> IO ()) -> IO Buffer
create p l f = f p >> return (mk_buffer p 0 l)



------------------------------------------------------------------------------
-- Instances                                                                --
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- Literal values                                                           --
------------------------------------------------------------------------------

ones64 :: Word64
ones64 = 0xffffffffffffffff --18446744073709551615

httpVersionPrefix64 :: Word64
httpVersionPrefix64 = 0x485454502f --"HTTP/"

httpVersionPrefix :: Ptr Word64 -> Buffer
httpVersionPrefix p = unsafeCreate p 5 (\p -> poke p httpVersionPrefix64)



------------------------------------------------------------------------------
-- Unsafe                                                                   --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Don't do memory allocation within an 'inlinePerformIO' block.

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r


unsafeCreate :: Ptr Word64 -> Int -> (Ptr Word64 -> IO ()) -> Buffer
unsafeCreate p l f = unsafeDupablePerformIO (create p l f)



------------------------------------------------------------------------------

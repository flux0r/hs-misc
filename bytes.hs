{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}

------------------------------------------------------------------------------
import Control.Category (Category ((.), id))
import Control.Monad (Monad ((>>), (>>=), return))
import Data.Bool (Bool (True, False), (&&), otherwise)
import Data.Eq (Eq ((/=), (==)))
import Data.Int (Int)
import Data.Ord (Ordering (EQ, GT, LT), compare, min)
import Data.Word (Word64)
import Foreign.C.Types (CInt (CInt), CSize (CSize))
import Foreign.Ptr (Ptr, plusPtr)
import GHC.Base (realWorld#)
import GHC.IO (IO (IO))
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
-- Data representation                                                      --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A Word64 vector.

data Bytes = B (Ptr Word64) Int Int

seek :: Bytes -> Ptr a
seek (B p i _) = plusPtr p i

len :: Bytes -> Int
len (B _ _ s) = s



------------------------------------------------------------------------------
-- Low-level construction and destruction                                   --
------------------------------------------------------------------------------


------------------------------------------------------------------------------

mk_bytes :: Ptr Word64 -> Int -> Int -> Bytes
mk_bytes = B


------------------------------------------------------------------------------

un_bytes :: Bytes -> (Ptr Word64, Int, Int)
un_bytes (B p i l) = (p, i, l)


------------------------------------------------------------------------------
-- | Create a Bytes of size l using Ptr p and then fill it with action f.

create :: Ptr Word64 -> Int -> (Ptr Word64 -> IO ()) -> IO Bytes
create p l f = f p >> return (mk_bytes p 0 l)



------------------------------------------------------------------------------
-- Instances                                                                --
------------------------------------------------------------------------------


------------------------------------------------------------------------------

instance Eq Bytes where
    (==) = eq_b

eq_b :: Bytes -> Bytes -> Bool
eq_b x@(B p0 i0 l0) y@(B p1 i1 l1)
    | l0 /= l1              = False
    | p0 == p1 && i0 == i1  = True
    | otherwise             = cmp_b x y == EQ


------------------------------------------------------------------------------

instance Ord Bytes where
    compare = cmp_b

cmp_b :: Bytes -> Bytes -> Ordering
cmp_b (B _ _ 0) (B _ _ 0) = EQ
cmp_b x y = inlinePerformIO (cmp >>= return . inspect)
  where
    cmp         = memcmp (seek x) (seek y) (min l0 l1)
    l0          = len x
    l1          = len y
    inspect x   = case compare x 0 of
        EQ              -> compare l0 l1
        x               -> x


------------------------------------------------------------------------------
-- Unsafe                                                                   --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Don't do memory allocation within an 'inlinePerformIO' block.

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r



------------------------------------------------------------------------------

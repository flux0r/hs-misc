{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}

import Control.Category (Category ((.), id))
import Control.Monad (Monad ((>>=), (>>), return))
import Data.Bool (Bool (True, False), (&&), otherwise)
import Data.Eq (Eq ((==), (/=)))
import Data.Ord (Ordering (GT, LT, EQ), compare, min)
import Data.Word (Word64, Word8)
import Foreign.C.Types (CSize (CSize), CULong (CULong))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Base (IO (IO), realWorld#)
import Prelude ((-), fromIntegral)

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CULong

memcmp :: Ptr Word8 -> Ptr Word8 -> Word64 -> IO CULong
memcmp x y s = c_memcmp x y (fromIntegral s)

data OctetVector = O (Ptr Word8) Word64 Word64

eq :: OctetVector -> OctetVector -> Bool
eq x@(O px ox lx) y@(O py oy ly)
    | lx /= ly              = False
    | px == py && ox == oy  = True
    | otherwise             = cmp x y == EQ

cmp :: OctetVector -> OctetVector -> Ordering
cmp (O _ _ 0) (O _ _ 0)         = EQ
cmp (O px ox lx) (O py oy ly)   = inlinePerformIO
    (memcmp ix iy s >>= return . r_culong)
  where
    ix          = plusPtr px (fromIntegral ox)
    iy          = plusPtr py (fromIntegral oy)
    s           = min lx ly
    r_culong i  = case compare i 0 of
        EQ  -> compare lx ly
        i   -> i

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

bounds :: OctetVector -> (Word64, Word64)
bounds (O _ o l) = (0, l - o)

unsafe_ref :: OctetVector -> Word8
unsafe_ref (O p o _) = inlinePerformIO (peekByteOff p (fromIntegral o))

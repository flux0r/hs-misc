{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}

------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Control.Category (Category ((.), id))
import Control.Exception (assert)
import Control.Monad (Monad ((>>), (>>=), return))
import Data.Bits ((.&.), shiftR)
import Data.Bool (Bool (True, False), (&&), not, otherwise)
import Data.Char (intToDigit)
import Data.Eq (Eq ((/=), (==)))
import Data.Function (($))
import Data.Int (Int)
import Data.Ord (Ord, Ordering (EQ, GT, LT), (>), (<), (<=), (>=), compare,
                 min)
import Data.Word (Word64)
import Foreign.C.Types (CInt (CInt), CSize (CSize))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peek, poke)
import Numeric (showIntAtBase)
import GHC.Base (realWorld#)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (IO (IO), unsafeDupablePerformIO)
import GHC.Prim (Addr#)
import GHC.Ptr (Ptr (Ptr))
import Prelude ((+), (*), (-), fromIntegral, putStrLn)
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
-- A Buffer is a Ptr Word64 payload, an Int offset in Word64s, and an Int
-- length in octets.

data Buffer = B (Ptr Word64) Int Int


------------------------------------------------------------------------------
-- | Http protocol version

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



parse_http_version p b =
    if is_prefix_of http_



------------------------------------------------------------------------------
-- Predicates                                                               --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Take two Buffers and test whether the first is a prefix of the second.

is_prefix_of :: Buffer -> Buffer -> Bool
is_prefix_of (B _ _ 0) _    = True
is_prefix_of (B p0 o0 l0) (B p1 o1 l1)
    | l1 < l0               = False
    | otherwise             = inline_perform_io $
        (== 0) <$> memcmp (seek p0 o0 l0) (seek p1 o1 l1) l0




------------------------------------------------------------------------------
-- | Test whether the first octet in a Word64 is zero.

is_0 :: Word64 -> Bool
is_0 x = x .&. 0xffff == 0x0


------------------------------------------------------------------------------
-- | Test whether the first octet in a Word64 is a digit.

is_digit :: Word64 -> Bool
is_digit x = let x0 = x .&. 0xffff in
    x0 >= 0x30 && x0 <= 0x39


------------------------------------------------------------------------------
-- | Test whether the first octet in a Word64 is a dot.

is_dot :: Word64 -> Bool
is_dot x = x .&. 0xffff == 0x46



------------------------------------------------------------------------------
-- Literal values and Buffers                                               --
------------------------------------------------------------------------------


ones64 :: Word64
ones64 = 0xffffffffffffffff --18446744073709551615


httpVersionPrefix64 :: Word64
httpVersionPrefix64 = 0x485454502f --"HTTP/"


http_version_prefix :: Ptr Word64 -> Buffer
http_version_prefix p = unsafe_create p 5 (\p' ->
    poke p' http_version_prefix_64)



------------------------------------------------------------------------------
-- Utilities                                                                --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Find the first index in a Buffer that satisfies a predicate. If no index
-- satisfies the predicate, just give the length of the Buffer.

find_index_or_end :: (Word64 -> Bool) -> Buffer -> Int
find_index_or_end pred (B p0 o0 l0) = inline_perform_io $
    iter (seek p0 o0 l0) 0
  where
    iter p n
        | n >= l0   = return l0
        | otherwise = peek p >>= \x ->
            if pred x
                then return n
                else iter (plusPtr p 1) (n + 1)


bin x = showIntAtBase 2 intToDigit x ""
hex x = showIntAtBase 16 intToDigit x ""
dec x = showIntAtBase 10 intToDigit x ""


------------------------------------------------------------------------------
-- Substrings                                                               --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Find the suffix remaining after dropping all consecutive elements from
-- the front of the Buffer that satisfy a predicate.

drop_while :: (Word64 -> Bool) -> Buffer -> Buffer
drop_while pred x = unsafe_drop (find_index_or_end (not . pred) x) x


------------------------------------------------------------------------------
-- | Find the longest prefix of a Buffer in which all the elements satisfy a
-- predicate, which could be empty if the first element doesn't satisfy the
-- predicate.

take_while :: (Word64 -> Bool) -> Buffer -> Buffer
take_while pred x = unsafe_take (find_index_or_end (not . pred) x) x


------------------------------------------------------------------------------
-- | Move the Buffer forward until the current element is not a zero.

skip_zeros :: Buffer -> Buffer
skip_zeros = drop_while is_0



------------------------------------------------------------------------------
-- Unsafe                                                                   --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Get a pointer to the first element of a buffer by passing the pointer in
-- the buffer, the Buffer offset in Word64s, and the Buffer length in octets.

seek :: Ptr Word64 -> Int -> Int -> Ptr Word64
seek p o l = plusPtr p (o*8 + (l - 8*(shiftR l 3)))


------------------------------------------------------------------------------
-- | Faster perfom IO for GHC. Do *not* do memory allocation within the IO
-- action; it will really fuck shit up.

inline_perform_io :: IO a -> a
inline_perform_io (IO m) = case m realWorld# of (# _, r #) -> r


------------------------------------------------------------------------------
-- | Make a Buffer outside the IO Monad.

unsafe_create :: Ptr Word64 -> Int -> (Ptr Word64 -> IO ()) -> Buffer
unsafe_create p l f = unsafeDupablePerformIO (create p l f)


------------------------------------------------------------------------------
-- | Drop n octets from the Buffer. There is no check that the argument n0 is
-- non-negative and less than or equal to the length of the Buffer. Do *not*
-- use this function unless you can prove the Buffer has a length less than or
-- equal to n0 and that n0 is non-negative.

unsafe_drop :: Int -> Buffer -> Buffer
unsafe_drop n0 (B p0 o0 l0) =
    assert (0 <= n0 && n0 <= l0)
           (B p0 (o0 + shiftR n0 3) (l0 - n0))


------------------------------------------------------------------------------
-- | Take n octets from the Buffer. There is no check that the argument n0 is
-- coherent. (See the comments about unsafe_drop to read the definition of
-- coherent.)

unsafe_take :: Int -> Buffer -> Buffer
unsafe_take n0 (B p0 o0 l0) =
    assert (0 <= n0 && n0 <= l0)
           (B p0 o0 n0)


starts_with x l b = assert (0 <= l && l <= 3) $
    case l of
        0       -> True
            



------------------------------------------------------------------------------

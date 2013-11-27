{-# LANGUAGE MagicHash, NoImplicitPrelude, RankNTypes, UnboxedTuples #-}

import Control.Category ((.))
import Control.Exception (assert)
import Control.Monad (Monad ((>>), (>>=), return))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Bool (Bool (True, False), (&&))
import Data.Char (intToDigit)
import Data.Either (Either (Left, Right))
import Data.Eq ((==))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Ord ((>=), (<=), (>))
import Data.String (String)
import Data.Word (Word64)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import GHC.Base (realWorld#)
import GHC.IO (IO (IO), unsafeDupablePerformIO)
import Numeric (showIntAtBase)
import Prelude ((-), (*), (+))

--                             -------- Offset in Word64s
--                            /  ------ Octet in current Word64
--                           /  /   --- Length in octets
--                          /  /   /
data Buf = B (Ptr Word64) Int Int Int

alloc_buf :: Int -> (Ptr Word64 -> IO ()) -> IO Buf
alloc_buf nword f = let l = nword * 8 in
    mallocBytes l >>= \p-> f p >> return (B p 0 0 l)

unsafe_alloc_buf :: Int -> (Ptr Word64 -> IO ()) -> Buf
unsafe_alloc_buf nword = unsafeDupablePerformIO . alloc_buf nword

select_octet :: Word64 -> Int -> Word64
select_octet x 0 = x .&. 0xff00000000000000
select_octet x 1 = x .&. 0x00ff000000000000
select_octet x 2 = x .&. 0x0000ff0000000000
select_octet x 3 = x .&. 0x000000ff00000000
select_octet x 4 = x .&. 0x00000000ff000000
select_octet x 5 = x .&. 0x0000000000ff0000
select_octet x 6 = x .&. 0x000000000000ff00
select_octet x 7 = x .&. 0x00000000000000ff

data Result r = Fail Buf [String] String
              | Cont (Buf -> Result r)
              | Done Buf r

newtype Input = I { get_input :: Buf }

newtype Added = A { get_added :: Buf }

data More = Nope | Yeah

newtype Failure r = Failure 
    { failure :: Input
              -> Added
              -> More
              -> [String]
              -> String
              -> Result r 
    }

newtype Success a r = Success
    { success :: Input
              -> Added
              -> More
              -> a
              -> Result r
    }

newtype Parser a = P
    { go_parser :: forall r. Input
                -> Added
                -> More
                -> Failure r
                -> Success a r
                -> Result r
    }

fmap_result :: (a -> b) -> Result a -> Result b
fmap_result _ (Fail buf ctx msg) = Fail buf ctx msg
fmap_result f (Cont k) = Cont (fmap_result f . k)
fmap_result f (Done buf r) = Done buf (f r)

octet_at :: Word64 -> Int -> Word64
octet_at x i = shiftL x ((7 - i) * 8)

take :: Int -> Buf -> Buf
take n (B p0 off0 oct0 l0) = B p0 off0 oct0 n

octet_with :: (Word64 -> Word64 -> a) -> Word64 -> Int -> Word64 -> a
octet_with f to i w =
    let to' = octet_at to i
        w'  = select_octet w i
    in  f to' w'

between :: Word64 -> Word64 -> Int -> Word64 -> Bool
between lo hi i x =
    let x'  = select_octet x i
        lo' = octet_at lo i
        hi' = octet_at hi i
    in  lo' <= x' && x' <= hi'

drop :: Int -> Buf -> Buf
drop n (B p0 off0 oct0 l0) =
    let start   = off0 * 8 + oct0
        off1    = shiftR start 3
    in  (B p0 off1 ((start + n) - off1) (l0 - n))

split_at :: Int -> Buf -> (Buf, Buf)
split_at n b = (take n b, drop n b)

take_with :: Int -> (Buf -> Bool) -> Buf -> Either String (Buf, Buf)
take_with n pred buf@(B p0 off0 oct0 l0) =
    let (h, t) = split_at n buf
    in  if pred h then Right (h, t) else Left "take_with"

drop_with :: Int -> (Buf -> Bool) -> Buf -> Either String Buf
drop_with n pred buf@(B p0 off0 oct0 l0) =
    let (h, t) = split_at n buf
    in  if pred h then Right t else Left "drop_with"

current_word :: Ptr Word64 -> Int -> Word64
current_word p off = inline_perform_io (peekByteOff p (8 * off))

get_word :: Buf -> Word64
get_word (B p0 off0 0 _) = inline_perform_io (peekByteOff p0 (8 * off0))
get_word (B p0 off0 oct0 l0) = inline_perform_io (do
    let offset  = off0 * 8
        shift   = oct0 * 8
    unaligned0 <- peekByteOff p0 offset
    unaligned1 <- peekByteOff p0 (offset + 8)
    return ((.|.) (shiftL unaligned0 shift)
                  (shiftR unaligned1 (64 - shift))))

inline_perform_io :: IO a -> a
inline_perform_io (IO m) = case m realWorld# of (# _, r #) -> r

show_num_base :: Word64 -> Word64 -> String
show_num_base b x = showIntAtBase b intToDigit x ""

bin, hex, dec :: Word64 -> String
bin = show_num_base 2
hex = show_num_base 16
dec = show_num_base 10

octet_eq :: Word64 -> Buf -> Bool
octet_eq x (B p off oct _) = octet_with (==) x oct (current_word p off)

octet_between :: Word64 -> Word64 -> Buf -> Bool
octet_between lo hi (B p off oct _) = between lo hi oct (current_word p off)

is_http_version_prefix :: Word64 -> Bool
is_http_version_prefix x = shiftR x 24 == 0x485454502f  -- "HTTP/"

is_digit :: Buf -> Bool
is_digit = octet_between 0x30 0x39

is_zero :: Buf -> Bool
is_zero = octet_eq 0x0

drop_http_prefix :: Buf -> Either String Buf
drop_http_prefix = drop_with 5 (is_http_version_prefix . get_word)

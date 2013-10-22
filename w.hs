{-# LANGUAGE MagicHash #-}

import Data.Word (Word64)
import Foreign.ForeignPtr (ForeignPtr (ForeignPtr), withForeignPtr)
import Foreign.Ptr (Ptr)
import GHC.Base (nullAddr#)
import GHC.IO (unsafeDupablePerformIO)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes, nullAddr#)

data Words = Ws
    { w         :: ForeignPtr Word64
    , len       :: Int
    , offset    :: Int
    , octets    :: Int
    }

mkWords :: Int -> (Ptr Word64 -> IO ()) -> IO Words
mkWords sz f = mallocPlainForeignPtrBytes sz >>= \ptr ->
    withForeignPtr ptr (\p -> f p) >> (return $! Ws ptr sz 0 0)

unsafeMkWords :: Int -> (Ptr Word64 -> IO ()) -> Words
unsafeMkWords l f = unsafeDupablePerformIO (mkWords l f)

nullForeignPtr = ForeignPtr nullAddr#
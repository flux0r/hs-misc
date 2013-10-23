{-# LANGUAGE MagicHash #-}

import Data.Word (Word64)
import Foreign.ForeignPtr (ForeignPtr (ForeignPtr), withForeignPtr)
import Foreign.Ptr (Ptr)
import GHC.Base (nullAddr#)
import GHC.IO (unsafeDupablePerformIO)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes, nullAddr#)

data Bytes = B
    { w         :: Ptr Word64
    , len       :: Int
    , offset    :: Int
    }

mkBytes :: Int -> (Ptr Word64 -> IO ()) -> IO Bytes
mkBytes sz f = mallocPlainForeignPtrBytes sz >>= \ptr ->
    withForeignPtr ptr (\p -> f p) >> (return $! B ptr sz 0)

unsafeMkBytes :: Int -> (Ptr Word64 -> IO ()) -> Bytes
unsafeMkBytes l f = unsafeDupablePerformIO (mkWords l f)

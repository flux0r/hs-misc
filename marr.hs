{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, NoImplicitPrelude,
             TypeFamilies #-}

import Control.Monad (Monad)
import Data.Word (Word, Word8)
import Foreign.Ptr (Ptr)
import Prelude (Integral, Real)

class (Real (Measure a)) => Measured a where
    type Measure a :: *
    measure :: a -> Measure a

class (Measured a, Integral (Idx a), Monad m) => MArray a m where
    type Idx a :: *
    type Elt a :: *
    bounds :: a -> m (Idx a, Idx a)
    length :: a -> m (Measure a)
    ref :: a -> Idx a -> m (Elt a)
    set :: a -> Idx a -> Elt a -> m ()
    fill :: a -> Elt a -> m ()

data OctetVector = O (Ptr Word8) Word Word

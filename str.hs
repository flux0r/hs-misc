{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude #-}

import Control.Monad (Monad)
import Data.Eq (Eq)
import Data.Word (Word64)

class (Monad m, Eq s, Ord s, Monoid s, Functor s) => MStr s m where
    type Elt s      :: *
    len             :: s -> m Word64
    ref             :: s -> Word64 -> m (Elt s)
    set             :: s -> Word64 -> Elt s -> m ()
    fill            :: s -> Elt s -> m ()
    mk_string       :: Word64 -> m s
    mk_string_fill  :: Word64 -> Elt s -> m s
    pack            :: [Elt s] -> m s
    substring       :: s -> Word64 -> Word64 -> m s
    unpack          :: s -> [Elt s]
    copy            :: s -> m s

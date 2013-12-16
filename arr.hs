{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, TypeFamilies #-}

import Prelude (Integral, Real)

class (Real (Measure a)) => Measured a where
    type Measure a :: *
    measure :: a -> Measure a

class (Measured a, Integral (Idx a)) => Array a where
    type Idx a :: *
    type Elt a :: *
    ref :: a -> Idx a -> Elt a
    set :: a -> Idx a -> Elt a -> a
    length :: a -> Measure a
    fill :: a -> Elt a -> a

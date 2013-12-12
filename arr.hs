{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}

class Array a where
    type Idx a :: *
    type Elt a :: *
    ref :: a -> Idx a -> Elt a
    set :: a -> Idx a -> Elt a -> a

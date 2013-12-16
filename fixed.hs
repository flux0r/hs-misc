{-# LANGUAGE NoImplicitPrelude, DataKinds, GADTs, KindSignatures, TypeOperators #-}

import GHC.TypeLits

data Vector (n :: Nat) a where
    Nil     :: Vector 0 a
    Cons    :: a -> Vector n a -> Vector (n + 1) a

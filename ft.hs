{-# LANGUAGE NoImplicitPrelude #-}

class Semigroup a where
    (<>) :: a -> a -> a

class Reducible f where
    reduce_r    :: (a -> b -> b) -> (f a -> b -> b)
    reduce_l    :: (b -> a -> b) -> (b -> f a -> b)

data Digit a = Digit a a a a

data Node a = DiNode a a | TriNode a a a

data FingerTree a = Nil
                  | Top a
                  | Next (Digit a) (FingerTree (Node a)) (Digit a)

instance Reducible Node where
    reduce_r f (DiNode x y) c       = f x (f y c)
    reduce_r f (TriNode x y z) c    = f x (f y (f z c))
    reduce_l f c (DiNode x y)       = f (f c x) y
    reduce_l f c (TriNode x y z)    = f (f (f c x) y) z

instance Reducible FingerTree

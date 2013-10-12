import Control.Lens
import Data.Bits
import Data.Word (Word32, Word64)

data PlainText = Pt Word64 Word64 deriving Show

lo :: Lens' PlainText Word64
lo = lens (\(Pt l _) -> l) (\(Pt _ h) l -> Pt l h)

hi :: Lens' PlainText Word64
hi = lens (\(Pt _ h) -> h) (\(Pt l _) h -> Pt l h)

instance Eq PlainText where
    x == y = x^.lo == y^.lo && x^.hi == y^.hi

ptAnd :: PlainText -> PlainText -> PlainText
ptAnd x y = Pt (x^.lo .&. y^.lo) (x^.hi .&. y^.hi)

ptOr :: PlainText -> PlainText -> PlainText
ptOr x y = Pt (x^.lo .|. y^.lo) (x^.hi .|. y^.hi)

ptXor :: PlainText -> PlainText -> PlainText
ptXor x y = Pt (x^.lo `xor` y^.lo) (x^.hi `xor` y^.hi)

ptComplement :: PlainText -> PlainText
ptComplement = (lo %~ complement) . (hi %~ complement)

ptShift :: PlainText -> Int -> PlainText
ptShift x 0 = x
ptShift x i =
    if i > 0
        then
            Pt (shift (x^.lo) i)
               ((shift (x^.hi) i) .|. (fromIntegral $ shift (x^.lo) (i - 64)))
        else
            Pt ((shift (x^.lo) i) .|. (fromIntegral $ shift (x^.hi) (i + 64)))
               (shift (x^.hi) i)

ptRotate :: PlainText -> Int -> PlainText
ptRotate x 0 = x
ptRotate x i =
    let o = if i > 0 then (-) else (+)
    in  ptOr (ptShift x i) (ptShift x (i `o` (bitSize x)))

instance Bits PlainText where
    (.&.)       = ptAnd
    (.|.)       = ptOr
    xor         = ptXor
    shift       = ptShift
    rotate      = ptRotate
    complement  = ptComplement
    bitSize x   = bitSize (x^.lo) + bitSize (x^.hi)
    isSigned _  = False
    bit i       = ptShift (Pt 1 0) i
    testBit x i = (x .&. bit i) /= (Pt 0 0)
    popCount x  = popCount (x^.lo) + popCount (x^.hi)

ptToInteger :: PlainText -> Integer
ptToInteger x =
    let h = (fromIntegral $ x^.hi) `shiftL` 64
        l = fromIntegral $ x^.lo
    in  h .|. l

integerToPt :: Integer -> PlainText
integerToPt x =
    let h = (fromIntegral $ x `shiftR` 64) .&. ones64
        l = (fromIntegral x) .&. ones64
    in  Pt l h

-- quotRemPt :: PlainText -> PlainText -> (PlainText, PlainText)
-- quotRemPt x y =
--     let r = x - q*y
--         q = iter 0 64 0
--     in  (q, r)
--   where
--     iter t 0 v = if v >= y then t + 1 else t
--     iter t i v =
--         if v >= y
--             then iter (setBit t i) i' v2
--             else iter t i' v1
--       where
--         i' = i - 1
--         newBit = if (testBit x i') then 1 else 0
--         v1 = (v `shiftL` 1) .|. newBit
--         v2 = ((v - y) `shiftL` 1) .|. newBit

ones64 :: Word64
ones64 = 0xffffffffffffffff

newtype Block = Bl (Word32, Word32, Word32, Word32) deriving Show

unBlock :: Lens' Block (Word32, Word32, Word32, Word32)
unBlock = lens (\(Bl x) -> x) (\_ x -> Bl x)

b0, b1, b2, b3 :: Lens' Block Word32
b0 = unBlock . _1
b1 = unBlock . _2
b2 = unBlock . _3
b3 = unBlock . _4

-- mkBlock :: PlainText -> Block
-- mkBlock x = let x' = fromIntegral x in
--     Bl (x' .&. ones,
--        (x' `shiftR` 32) .&. ones,
--        (x' `shiftR` 64) .&. ones,
--        (x' `shiftR` 96) .&. ones)
--   where
--     ones = 0xffffffff

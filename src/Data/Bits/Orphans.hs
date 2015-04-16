{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Bits.Orphans () where

-- These instances are only valid if Bits isn't a subclass of Num (as Bool is
-- not a Num instance), which is only true as of base-4.6.0.0 and later.
#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
import Data.Bits

instance Bits Bool where
    (.&.) = (&&)

    (.|.) = (||)

    xor = (/=)

    complement = not

    shift x 0 = x
    shift _ _ = False

    rotate x _ = x

    bit 0 = True
    bit _ = False

    testBit x 0 = x
    testBit _ _ = False

    bitSize _ = 1

    isSigned _ = False

    popCount False = 0
    popCount True  = 1
#endif

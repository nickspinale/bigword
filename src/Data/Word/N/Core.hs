{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ConstraintKinds            #-}

---------------------------------------------------------
-- |
-- Module      : Data.Word.N.Core
-- Copyright   : (c) 2015 Nick Spinale
-- License     : MIT
--
-- Maintainer  : Nick Spinale <spinalen@carleton.edu>
-- Stability   : provisional
-- Portability : portable
--
-- Core types and operations of the package.
---------------------------------------------------------

module Data.Word.N.Core
    ( W
    , BothKnown
    , Triplet
    , (>+<)
    , split
    ) where

import Data.Bits
import Data.Data
import Data.Function
import Data.Ix
import GHC.Exts
import GHC.TypeLits
import Numeric.Mod
import Text.Printf

-- | Synonym for readability
type BothKnown n = (KnownNat n, KnownNat (2 ^ n))

-- | Synonym for readability
type Triplet m n o = ( BothKnown m
                     , BothKnown n
                     , BothKnown o
                     , (m + n) ~ o
                     , (n + m) ~ o
                     )

-- | Type representing a sequence of @n@ bits, or a non-negative integer smaller than @2^n@.

-- Original name was BigWord, but since using this module requires more
-- explicit type signatures, I decided to use just W. This may be stupid.
-- Sorry if the name conflicts with your code.

newtype W (n :: Nat) = W { unW :: Mod (2 ^ n) }
    deriving (Eq, Enum, Ord, Ix, PrintfArg, Typeable)

deriving instance KnownNat (2 ^ n) => Integral (W n)
deriving instance KnownNat (2 ^ n) => Real (W n)
deriving instance KnownNat (2 ^ n) => Bounded (W n)
deriving instance KnownNat (2 ^ n) => Num (W n)

deriving instance (Typeable n, Typeable (2 ^ n)) => Data (W n)

-------------------------------
-- INSTANCES
-------------------------------

instance Show (W n) where
    show = show . unW

instance Read (W n) where
    readsPrec = ((.).(.)) (map $ \(a, str) -> (W a, str)) readsPrec

instance BothKnown n => Bits (W n) where
        (.&.) = ((.).(.)) fromInteger ((.&.) `on` toInteger)
        (.|.) = ((.).(.)) fromInteger ((.|.) `on` toInteger)
        xor   = ((.).(.)) fromInteger (xor   `on` toInteger)
        complement = fromInteger . complement . toInteger
        shift w i = fromInteger $ shift (toInteger w) i
        rotate w i = let nat = natValInt' (proxy# :: Proxy# n)
                         dist = i `mod` nat
                     in shift w dist .|. shift w (dist - nat)
        bitSizeMaybe = Just . finiteBitSize
        bitSize = finiteBitSize
        isSigned = const False
        testBit = testBit . toInteger
        bit i = if i < natValInt' (proxy# :: Proxy# n)
                then fromInteger (bit i)
                else 0
        popCount = popCount . toInteger

instance BothKnown n => FiniteBits (W n) where
    finiteBitSize = const $ natValInt' (proxy# :: Proxy# n)

-------------------------------
-- OPERATIONS
-------------------------------

-- | Appends two @'W'@'s, treating the first's bits as more significant.
--
-- Example usage:
--
-- >    import Network.Socket
-- >
-- >    fromHostAddress6 :: HostAddress6 -> W 128
-- >    fromHostAddress6 (a, b, c, d) = f a >+< f b >+< f c >+< f d
-- >      where
-- >        f = fromIntegral :: Word32 -> W 32

infixr 6 >+<

(>+<) :: forall m n o. ( BothKnown m
                       , BothKnown n
                       , BothKnown o
                       , (m + n) ~ o
                       ) => W m -> W n -> W o

(W x) >+< (W y) = fromInteger $ shift (toInteger x) (natValInt' (proxy# :: Proxy# n)) .|. toInteger y

-- | The inverse of @'>+<'@
--
-- >    forall a b. split (a >+< b) == (a, b)
--
-- Example usage:
--
-- >    import Network.Socket
-- >
-- >    toHostAddress6 :: W 128 -> HostAddress6
-- >    toHostAddress6 w =  (f a, f b, f c, f d)
-- >      where
-- >        f = fromIntegral :: W 32 -> Word32
-- >        (a, x) = split w
-- >        (b, y) = split x
-- >        (c, d) = split y

split :: forall m n o. ( BothKnown m
                       , BothKnown n
                       , BothKnown o
                       , (m + n) ~ o
                       ) => W o -> (W m, W n)
split (W z) = (fromInteger $ shiftR (toInteger z) (natValInt' (proxy# :: Proxy# n)), fromIntegral z)

-------------------------------
-- HELPERS
-------------------------------

natValInt' :: KnownNat n => Proxy# n -> Int
natValInt' p = fromInteger $ natVal' p

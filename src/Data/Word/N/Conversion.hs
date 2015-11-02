{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

---------------------------------------------------------
-- |
-- Module      : Data.Word.N.Internal.Conversion
-- Copyright   : (c) 2015 Nick Spinale
-- License     : MIT
--
-- Maintainer  : Nick Spinale <spinalen@carleton.edu>
-- Stability   : provisional
-- Portability : portable
--
-- Classes for converting to and from the @'W' n@ type.
-- @'ToW'@ and @'FromW'@ interpret the leftmost members of tuples as the most significant.
---------------------------------------------------------

module Data.Word.N.Conversion
    ( ToW(..)
    , FromW(..)
    , AccSums
    ) where

import Data.Word.N.Core

import Data.Int
import Data.Type.List
import Data.Word
import GHC.Exts
import GHC.TypeLits

-- | Provides a way to convert from fixed-bit types to the @'W' n@ type.
class ToW n a | a -> n where
    toW :: a -> W n

instance ToW n (W n) where
    toW = id

instance ToW  8 Word8  where toW = fromIntegral
instance ToW 16 Word16 where toW = fromIntegral
instance ToW 32 Word32 where toW = fromIntegral
instance ToW 64 Word64 where toW = fromIntegral
instance ToW  8 Int8   where toW = fromIntegral
instance ToW 16 Int16  where toW = fromIntegral
instance ToW 32 Int32  where toW = fromIntegral
instance ToW 64 Int64  where toW = fromIntegral

-- | Provides a way to convert from the @'W' n@ type to fixed-bit types.
class FromW n a | a -> n where
    fromW :: W n -> a

instance FromW n (W n) where
    fromW = id

instance FromW  8 Word8  where fromW = fromIntegral
instance FromW 16 Word16 where fromW = fromIntegral
instance FromW 32 Word32 where fromW = fromIntegral
instance FromW 64 Word64 where fromW = fromIntegral
instance FromW  8 Int8   where fromW = fromIntegral
instance FromW 16 Int16  where fromW = fromIntegral
instance FromW 32 Int32  where fromW = fromIntegral
instance FromW 64 Int64  where fromW = fromIntegral

-- | Convenience constraint synonym.
-- Tuple instances of @'ToW'@ and @'FromW'@ would be a lot uglier without it.
type family AccSums (list :: [Nat]) :: Constraint where
    AccSums '[] = ()
    AccSums (n ': ns) = (Triplet n (Sum ns) (Sum (n ': ns)), AccSums ns)

--------------------------------
-- ToW instances for tuples
--------------------------------

instance ( ToW n0 a0
         , ToW n1 a1
         , AccSums [n0, n1]
         , n ~ Sum [n0, n1]
         ) => ToW n (a0, a1) where
    toW (a0, a1) = toW a0 >+< toW a1

instance ( ToW n0 a0
         , ToW n1 a1
         , ToW n2 a2
         , AccSums [n0, n1, n2]
         , n ~ Sum [n0, n1, n2]
         ) => ToW n (a0, a1, a2) where
    toW (a0, a1, a2) = toW a0 >+< toW a1 >+< toW a2

instance ( ToW n0 a0
         , ToW n1 a1
         , ToW n2 a2
         , ToW n3 a3
         , AccSums [n0, n1, n2, n3]
         , n ~ Sum [n0, n1, n2, n3]
         ) => ToW n (a0, a1, a2, a3) where
    toW (a0, a1, a2, a3) = toW a0 >+< toW a1 >+< toW a2 >+< toW a3

instance ( ToW n0 a0
         , ToW n1 a1
         , ToW n2 a2
         , ToW n3 a3
         , ToW n4 a4
         , AccSums [n0, n1, n2, n3, n4]
         , n ~ Sum [n0, n1, n2, n3, n4]
         ) => ToW n (a0, a1, a2, a3, a4) where
    toW (a0, a1, a2, a3, a4) = toW a0 >+< toW a1 >+< toW a2 >+< toW a3 >+< toW a4

instance ( ToW n0 a0
         , ToW n1 a1
         , ToW n2 a2
         , ToW n3 a3
         , ToW n4 a4
         , ToW n5 a5
         , AccSums [n0, n1, n2, n3, n4, n5]
         , n ~ Sum [n0, n1, n2, n3, n4, n5]
         ) => ToW n (a0, a1, a2, a3, a4, a5) where
    toW (a0, a1, a2, a3, a4, a5) = toW a0 >+< toW a1 >+< toW a2 >+< toW a3 >+< toW a4 >+< toW a5

instance ( ToW n0 a0
         , ToW n1 a1
         , ToW n2 a2
         , ToW n3 a3
         , ToW n4 a4
         , ToW n5 a5
         , ToW n6 a6
         , AccSums [n0, n1, n2, n3, n4, n5, n6]
         , n ~ Sum [n0, n1, n2, n3, n4, n5, n6]
         ) => ToW n (a0, a1, a2, a3, a4, a5, a6) where
    toW (a0, a1, a2, a3, a4, a5, a6) = toW a0 >+< toW a1 >+< toW a2 >+< toW a3 >+< toW a4 >+< toW a5 >+< toW a6

instance ( ToW n0 a0
         , ToW n1 a1
         , ToW n2 a2
         , ToW n3 a3
         , ToW n4 a4
         , ToW n5 a5
         , ToW n6 a6
         , ToW n7 a7
         , AccSums [n0, n1, n2, n3, n4, n5, n6, n7]
         , n ~ Sum [n0, n1, n2, n3, n4, n5, n6, n7]
         ) => ToW n (a0, a1, a2, a3, a4, a5, a6, a7) where
    toW (a0, a1, a2, a3, a4, a5, a6, a7) = toW a0 >+< toW a1 >+< toW a2 >+< toW a3 >+< toW a4 >+< toW a5 >+< toW a6 >+< toW a7

instance ( ToW n0 a0
         , ToW n1 a1
         , ToW n2 a2
         , ToW n3 a3
         , ToW n4 a4
         , ToW n5 a5
         , ToW n6 a6
         , ToW n7 a7
         , ToW n8 a8
         , AccSums [n0, n1, n2, n3, n4, n5, n6, n7, n8]
         , n ~ Sum [n0, n1, n2, n3, n4, n5, n6, n7, n8]
         ) => ToW n (a0, a1, a2, a3, a4, a5, a6, a7, a8) where
    toW (a0, a1, a2, a3, a4, a5, a6, a7, a8) = toW a0 >+< toW a1 >+< toW a2 >+< toW a3 >+< toW a4 >+< toW a5 >+< toW a6 >+< toW a7 >+< toW a8

--------------------------------
-- FromW instances for tuples
--------------------------------

instance ( FromW n0 a0
         , FromW n1 a1
         , AccSums [n0, n1]
         , n ~ Sum [n0, n1]
         ) => FromW n (a0, a1) where
    fromW a = case split a
              of (a0, a1) -> (fromW a0, fromW a1)

instance ( FromW n0 a0
         , FromW n1 a1
         , FromW n2 a2
         , AccSums [n0, n1, n2]
         , n ~ Sum [n0, n1, n2]
         ) => FromW n (a0, a1, a2) where
    fromW a = case (fmap split . split) a
              of (a0, (a1, a2)) -> (fromW a0, fromW a1, fromW a2)

instance ( FromW n0 a0
         , FromW n1 a1
         , FromW n2 a2
         , FromW n3 a3
         , AccSums [n0, n1, n2, n3]
         , n ~ Sum [n0, n1, n2, n3]
         ) => FromW n (a0, a1, a2, a3) where
    fromW a = case (fmap (fmap split . split) . split) a
              of (a0, (a1, (a2, a3))) -> (fromW a0, fromW a1, fromW a2, fromW a3)

instance ( FromW n0 a0
         , FromW n1 a1
         , FromW n2 a2
         , FromW n3 a3
         , FromW n4 a4
         , AccSums [n0, n1, n2, n3, n4]
         , n ~ Sum [n0, n1, n2, n3, n4]
         ) => FromW n (a0, a1, a2, a3, a4) where
    fromW a = case (fmap (fmap (fmap split . split) . split) . split) a
              of (a0, (a1, (a2, (a3, a4)))) -> (fromW a0, fromW a1, fromW a2, fromW a3, fromW a4)

instance ( FromW n0 a0
         , FromW n1 a1
         , FromW n2 a2
         , FromW n3 a3
         , FromW n4 a4
         , FromW n5 a5
         , AccSums [n0, n1, n2, n3, n4, n5]
         , n ~ Sum [n0, n1, n2, n3, n4, n5]
         ) => FromW n (a0, a1, a2, a3, a4, a5) where
    fromW a = case (fmap (fmap (fmap (fmap split . split) . split) . split) . split) a
              of (a0, (a1, (a2, (a3, (a4, a5))))) -> (fromW a0, fromW a1, fromW a2, fromW a3, fromW a4, fromW a5)

instance ( FromW n0 a0
         , FromW n1 a1
         , FromW n2 a2
         , FromW n3 a3
         , FromW n4 a4
         , FromW n5 a5
         , FromW n6 a6
         , AccSums [n0, n1, n2, n3, n4, n5, n6]
         , n ~ Sum [n0, n1, n2, n3, n4, n5, n6]
         ) => FromW n (a0, a1, a2, a3, a4, a5, a6) where
    fromW a = case (fmap (fmap (fmap (fmap (fmap split . split) . split) . split) . split) . split) a
              of (a0, (a1, (a2, (a3, (a4, (a5, a6)))))) -> (fromW a0, fromW a1, fromW a2, fromW a3, fromW a4, fromW a5, fromW a6)

instance ( FromW n0 a0
         , FromW n1 a1
         , FromW n2 a2
         , FromW n3 a3
         , FromW n4 a4
         , FromW n5 a5
         , FromW n6 a6
         , FromW n7 a7
         , AccSums [n0, n1, n2, n3, n4, n5, n6, n7]
         , n ~ Sum [n0, n1, n2, n3, n4, n5, n6, n7]
         ) => FromW n (a0, a1, a2, a3, a4, a5, a6, a7) where
    fromW a = case (fmap (fmap (fmap (fmap (fmap (fmap split . split) . split) . split) . split) . split) . split) a
              of (a0, (a1, (a2, (a3, (a4, (a5, (a6, a7))))))) -> (fromW a0, fromW a1, fromW a2, fromW a3, fromW a4, fromW a5, fromW a6, fromW a7)

instance ( FromW n0 a0
         , FromW n1 a1
         , FromW n2 a2
         , FromW n3 a3
         , FromW n4 a4
         , FromW n5 a5
         , FromW n6 a6
         , FromW n7 a7
         , FromW n8 a8
         , AccSums [n0, n1, n2, n3, n4, n5, n6, n7, n8]
         , n ~ Sum [n0, n1, n2, n3, n4, n5, n6, n7, n8]
         ) => FromW n (a0, a1, a2, a3, a4, a5, a6, a7, a8) where
    fromW a = case (fmap (fmap (fmap (fmap (fmap (fmap (fmap split . split) . split) . split) . split) . split) . split) . split) a
              of (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, a8)))))))) -> (fromW a0, fromW a1, fromW a2, fromW a3, fromW a4, fromW a5, fromW a6, fromW a7, fromW a8)


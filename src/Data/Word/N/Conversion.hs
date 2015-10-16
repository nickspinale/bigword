{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE FunctionalDependencies     #-}

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
---------------------------------------------------------

module Data.Word.N.Conversion
    ( ToW(..)
    , FromW(..)
    , BigEndian(..)
    , LittleEndian(..)
    ) where

import Data.Word.N.Core

import Data.Int
import Data.Word
import GHC.TypeLits

-- | Provides a way to convert from fixed-bit types to the @'W' n@ type.
class ToW (n :: Nat) (a :: *) | a -> n where
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
class FromW (n :: Nat) (a :: *) | a -> n where
    fromW :: a -> W n

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

-- | Wrapper for tuples. @'ToW'@ and @'FromW'@ interpret the leftmost members of such tuples as the most significant.
newtype BigEndian    a = BigEndian    { fromBigEndian    :: a }

-- | Wrapper for tuples. @'ToW'@ and @'FromW'@ interpret the rightmost members of such tuples as the most significant.
newtype LittleEndian a = LittleEndian { fromLittleEndian :: a }

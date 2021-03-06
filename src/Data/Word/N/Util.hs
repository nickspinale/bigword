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
-- Module      : Data.Word.N.Util
-- Copyright   : (c) 2015 Nick Spinale
-- License     : MIT
--
-- Maintainer  : Nick Spinale <spinalen@carleton.edu>
-- Stability   : provisional
-- Portability : portable
--
-- Useful functions for working with @'W' n@'s.
---------------------------------------------------------

module Data.Word.N.Util
    (
      slice
    -- * Focused Operations
    , (:|:)(..)
    , assembleL
    , assembleR
    , disassembleL
    , disassembleR
    -- * Displaying
    , showBits
    , showHex
    ) where

import Data.Word.N
import Control.Applicative
import Data.Monoid
import Data.Proxy
import Data.Traversable
import Data.Type.Equality
import Data.Type.Bool
import GHC.TypeLits

-- NOTE: The order of these two constraints produces different behavior.
--       When switched the 'o' in 'n + o' is different than the one brought
--       into scope by the 'forall'.
-- TODO: Find out whether this is supposed to be the case, and if it is,
--       find out what part of the typechecker I don't understand.
slice :: forall m n o no mno. ( Triplet n o no
                              , Triplet m no mno
                              ) => Proxy o -> W mno -> W n
slice _ w = down
  where
    (_, up) = (split :: W mno -> (W m, W no)) w
    (down, _) = (split :: W no -> (W n, W o)) up

-------------------------------
-- COOL FUNCTIONS
-------------------------------

-- | Transforms an applicative action that results in a @'W' d@ to on that results in a @'W' n@, provided that @d|n@ (hence the @:|:@ constraint), treating the first results as more significant.
--
-- > assembleR = 'assemble' ('>+<')
--
-- Example using attoparsec to parse a big-endian unsigned 128-bit integer:
--
-- >    import Data.Attoparsec.ByteString
-- >    import Data.Word
-- >
-- >    anyWord128BE :: Parser (W 128)
-- >    anyWord128BE = assembleR $ fmap (fromIntegral :: Word8 -> W 8) anyWord8

assembleR :: (Applicative f, d :|: n) => f (W d) -> f (W n)
assembleR = assemble (>+<)

-- | Same as assembleR, but treats the first results as less significant.
--
-- > assembleL = 'assemble' (flip ('>+<'))
--
-- Here's the example above, modified to parse in little-endian byte order:
--
-- >    import Data.Attoparsec.ByteString
-- >    import Data.Word
-- >
-- >    anyWord128LE :: Parser (W 128)
-- >    anyWord128LE = assembleL $ fmap (fromIntegral :: Word8 -> W 8) anyWord8

assembleL :: (Applicative f, d :|: n) => f (W d) -> f (W n)
assembleL = assemble (flip (>+<))

-- | Breaks a @'W' n@ into its constituent @d@-sized chunks, and combines them according to the provided monoid.
--   More significant chunks are combined first.
--
-- > disassembleR = disassemble
--
-- Example using a bytestrings
--
-- >    import Data.ByteString.Builder
-- >
-- >    word128BE :: W 128 -> Builder
-- >    word128BE = disassembleR (word8 . (fromIntegral :: W 8 -> Word8))

disassembleR :: (Monoid m , d :|: n) => (W d -> m) -> (W n -> m)
disassembleR = disassemble

-- | Same as disassembleL, but combines less significant chunks first.
--
-- > disassembleL f = getDual . disassemble (Dual . f)
--
-- @disassembleR@'s example adjusted to build in little-endian byte order:
--
-- >    import Data.ByteString.Builder
-- >
-- >    word128LE :: W 128 -> Builder
-- >    word128LE = disassembleL (word8 . (fromIntegral :: W 8 -> Word8))

disassembleL :: (Monoid m , d :|: n) => (W d -> m) -> (W n -> m)
disassembleL f = getDual . disassemble (Dual . f)

-------------------------------
-- :|:
-------------------------------

-- | Reads "d divides n", and allows for generic construction and inpsection.

class (KnownNat d, KnownNat n) => d :|: n where

    -- | Construct an applicative action that results in a @'W' n@ from one that results in a @'W' d@, where @d | n@.
    -- See 'assembleR' and 'assembleL' for examples.
    assemble :: forall f. Applicative f
             => (forall a b c. (Triplet a b c, Triplet b a c) => W a -> W b -> W c)
             -> f (W d)
             -> f (W n)

    -- | Construct a means of pushing a @'W' n@ into a monoid from a means of pushing a @'W' d@ into the monoid, where @d | n@.
    -- See 'disassembleR' and 'disassembleL' for examples.
    disassemble :: forall m. Monoid m => (W d -> m) -> (W n -> m)

-- This sort of intermediate class is necessary because the 'Nat' kind lacks structure;
-- it cannot be matched on. There are other possible solutions, such as some sort of
-- alternative representation of naturals (data Nat = Z | S Nat) that wraps the actual
-- Nat kind (with type families for conversion) in moments like this. So, the actual
-- implementation of :|: will probably change, but its behavior will stay the same.
instance (eq ~ (d == n), Divides eq d n) => d :|: n where
    assemble = assemble'
    disassemble = disassemble'

class (KnownNat d, KnownNat n, eq ~ (d == n)) => Divides (eq :: Bool) (d :: Nat) (n :: Nat) where

    assemble' :: forall f. Applicative f
             => (forall a b c. (Triplet a b c, Triplet b a c) => W a -> W b -> W c)
             -> f (W d)
             -> f (W n)

    disassemble' :: forall m. Monoid m => (W d -> m) -> (W n -> m)


instance {-# OVERLAPPING #-} (KnownNat d, KnownNat n, True ~ (d == n), d ~ n) => Divides True d n where
    assemble' _ = id
    disassemble' = id

instance {-# OVERLAPPABLE #-} ( Triplet d n' n
                              , Triplet n' d n
                              , Divides (d == n') d n'
                              , False ~ (d == n)
                              ) => Divides False d n where

    assemble' c f = liftA2 c f (assemble' c f)

    disassemble' f w = f l <> disassemble' f r
      where
        l :: W d
        r :: W n'
        (l, r) = split w

------------------
-- DISPLAYING
------------------

-- | Displays a bitvector's bits in big-endian order.
showBits :: (1 :|: n) => W n -> String
showBits = disassembleR (show :: W 1 -> String)

-- | Displays a bitvector's bits in big-endian order, in groups of four as hexadecimal digits.
showHex :: (4 :|: n) => W n -> String
showHex = disassembleR $ (tokens !!) . (fromIntegral :: W 4 -> Int)
  where tokens = map (:[]) "0123456789ABCDEF"


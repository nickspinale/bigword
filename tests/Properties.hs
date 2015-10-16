{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

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

module Main (main) where

import Test.HUnit hiding (Test)
import Test.QuickCheck hiding ((.&.))
import Test.Framework ( Test, defaultMain )
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Data.Word.N
import Data.Word.N.Util
import Data.Bits
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LZ

import GHC.TypeLits
import Data.Word

-- Most of these properties come from the package 'largeword' (on hackage).
-- This is because that package and this one expose pretty much the same api.

encode' :: (8 :|: n) => W n -> LZ.ByteString
encode' = disassembleR (runPut . putWord8 . (fromIntegral :: W 8 -> Word8))

decode' :: (8 :|: n) => LZ.ByteString -> W n
decode' = runGet (assembleR (fmap (fromIntegral :: Word8 -> W 8) getWord8))

instance (8 :|: n) => Arbitrary (W n) where
   arbitrary = assembleL $ fmap (fromIntegral :: (Word8 -> W 8)) arbitrary

pShiftRightShiftLeft :: W 128 -> Bool
pShiftRightShiftLeft x = shiftR (shiftL x 1) 1 == x .&. (fromInteger ((2^127) - 1))

u1 :: Assertion
u1 = shiftR (18446744073709551616  :: W 128) 64  @?= 1

pQuotRem :: W 256 -> Bool
pQuotRem x = rx == fromInteger ry
  where
    (_qx, rx) = quotRem x 16
    (_qy, ry) = quotRem ((fromIntegral x) :: Integer) 16

encodeDecode :: (8 :|: n) => W n -> Bool
encodeDecode word = decode' encoded == word
  where
    encoded = encode' word
    {-# NOINLINE encoded #-}

correctEncoding :: Assertion
correctEncoding = (decode' . LZ.pack)
    [0,0,0,0,0,0,0,0,50,89,125,125,237,119,73,240
        ,217,12,178,101,235,8,44,221,50,122,244,125,115,181,239,78]
    @?=
    (1234567891234567891234567812345678123456781234567812345678 :: W 256)

pRotateLeftRight :: W 256 -> Bool
pRotateLeftRight x = rotate (rotate x 8) (-8) == x

pRepeatedShift :: Int -> Property
pRepeatedShift n =
  (n >= 0) && (n <= 1024) ==>
  (((iterate (`shift` 8) (1::W 192))!!n) == shift (1::W 192) (n*8))

pRepeatedShift' :: Int -> Property
pRepeatedShift' n =
  (n >= 0) && (n <= 1024) ==>
  (((iterate (`shift` 8) a)!!n) == shift a (n*8))
  where a :: W 192
        a = 0x0123456789ABCDEFFEDCBA98765432100011223344556677

pRepeatedShift160 :: Int -> Property
pRepeatedShift160 n =
  (n >= 0) && (n <= 1024) ==>
  (((iterate (`shift` 8) (1::W 160))!!n) == shift (1::W 160) (n*8))

u2 :: Assertion
u2 = (2 :: W (256 + 128)) ^ 254 @?=
     (fromInteger (2 :: Integer) ^ 254)

u3 :: Assertion
u3 = rotate (rotate ((2^255) :: W 256) (1)) (-1) @?=
     ((2^255) :: W 256)

u4 :: Assertion
u4 = shift (0x0123456789ABCDEFFEDCBA98765432100011223344556677 :: W 192) 80 @?=
           (0xBA9876543210001122334455667700000000000000000000 :: W 192)

u5 :: Assertion
u5 = shift (0x112233445566778899AABBCC :: W 96) 40 @?=
           (0x66778899AABBCC0000000000 :: W 96)

u6 :: Assertion
u6 = rotate ((2^95) :: W 96) (1) @?= 1

tests :: [Test]
tests =
    [ testProperty "largeword shift left then right" pShiftRightShiftLeft
    , testProperty "largeword quotRem by 16" pQuotRem
    , testProperty "largeword rotate left then right" pRotateLeftRight
    , testProperty "largeword repeated shift vs single shift" pRepeatedShift
    , testProperty "largeword repeated shift vs single shift" pRepeatedShift'
    , testProperty "largeword repeated shift vs single shift" pRepeatedShift160
    , testCase "largeword shift 2^64 by 2^64" u1
    , testCase "largeword exponentiation 2^254" u2
    , testCase "largeword rotation by 1" u3
    , testCase "largeword shift by 80" u4
    , testCase "largeword shift by 40" u5
    , testCase "largeword rotate by 1" u6
    , testCase "big-endian encoding" correctEncoding
    , testProperty "Word96 encode/decode loop"  (encodeDecode::W 96 -> Bool)
    , testProperty "Word128 encode/decode loop" (encodeDecode::W 128 -> Bool)
    , testProperty "Word160 encode/decode loop" (encodeDecode::W 160 -> Bool)
    , testProperty "Word192 encode/decode loop" (encodeDecode::W 192 -> Bool)
    , testProperty "Word224 encode/decode loop" (encodeDecode::W 224 -> Bool)
    , testProperty "Word256 encode/decode loop" (encodeDecode::W 256 -> Bool)
      ]

main :: IO ()
main = defaultMain tests

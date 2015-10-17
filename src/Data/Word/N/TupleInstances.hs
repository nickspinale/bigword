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
-- Module      : Data.Word.N.TupleInstances
-- Copyright   : (c) 2015 Nick Spinale
-- License     : MIT
--
-- Maintainer  : Nick Spinale <spinalen@carleton.edu>
-- Stability   : provisional
-- Portability : portable
--
-- Instances of @'ToW'@ and @'FromW'@ for tuples.
---------------------------------------------------------

module Data.Word.N.TupleInstances () where

import Data.Word.N.Core
import Data.Word.N.Conversion
 
import GHC.TypeLits

--------------------
-- ToW, BigEndian
--------------------

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)

         ) => ToW n01 (BigEndian (a0, a1)) where
    toW (BigEndian (a1, a0)) = toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)

         ) => ToW n012 (BigEndian (a2, a1, a0)) where
    toW (BigEndian (a2, a1, a0)) = toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)

         ) => ToW n0123 (BigEndian (a3, a2, a1, a0)) where
    toW (BigEndian (a3, a2, a1, a0)) = toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)

         ) => ToW n01234 (BigEndian (a4, a3, a2, a1, a0)) where
    toW (BigEndian (a4, a3, a2, a1, a0)) = toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)

         ) => ToW n012345 (BigEndian (a5, a4, a3, a2, a1, a0)) where
    toW (BigEndian (a5, a4, a3, a2, a1, a0)) = toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)

         ) => ToW n0123456 (BigEndian (a6, a5, a4, a3, a2, a1, a0)) where
    toW (BigEndian (a6, a5, a4, a3, a2, a1, a0)) = toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)
         , n01234567 ~ (n0123456 + n7), n01234567 ~ (n7 + n0123456), KnownNat n01234567, KnownNat (2 ^ n01234567)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)
         , ToW n7 a7, KnownNat n7, KnownNat (2 ^ n7)

         ) => ToW n01234567 (BigEndian (a7, a6, a5, a4, a3, a2, a1, a0)) where
    toW (BigEndian (a7, a6, a5, a4, a3, a2, a1, a0)) = toW a7 >+< toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)
         , n01234567 ~ (n0123456 + n7), n01234567 ~ (n7 + n0123456), KnownNat n01234567, KnownNat (2 ^ n01234567)
         , n012345678 ~ (n01234567 + n8), n012345678 ~ (n8 + n01234567), KnownNat n012345678, KnownNat (2 ^ n012345678)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)
         , ToW n7 a7, KnownNat n7, KnownNat (2 ^ n7)
         , ToW n8 a8, KnownNat n8, KnownNat (2 ^ n8)

         ) => ToW n012345678 (BigEndian (a8, a7, a6, a5, a4, a3, a2, a1, a0)) where
    toW (BigEndian (a8, a7, a6, a5, a4, a3, a2, a1, a0)) = toW a8 >+< toW a7 >+< toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--------------------
-- ToW, LittleEndian
--------------------

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)

         ) => ToW n01 (LittleEndian (a0, a1)) where
    toW (LittleEndian (a0, a1)) = toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)

         ) => ToW n012 (LittleEndian (a0, a1, a2)) where
    toW (LittleEndian (a0, a1, a2)) = toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)

         ) => ToW n0123 (LittleEndian (a0, a1, a2, a3)) where
    toW (LittleEndian (a0, a1, a2, a3)) = toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)

         ) => ToW n01234 (LittleEndian (a0, a1, a2, a3, a4)) where
    toW (LittleEndian (a0, a1, a2, a3, a4)) = toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)

         ) => ToW n012345 (LittleEndian (a0, a1, a2, a3, a4, a5)) where
    toW (LittleEndian (a0, a1, a2, a3, a4, a5)) = toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)

         ) => ToW n0123456 (LittleEndian (a0, a1, a2, a3, a4, a5, a6)) where
    toW (LittleEndian (a0, a1, a2, a3, a4, a5, a6)) = toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)
         , n01234567 ~ (n0123456 + n7), n01234567 ~ (n7 + n0123456), KnownNat n01234567, KnownNat (2 ^ n01234567)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)
         , ToW n7 a7, KnownNat n7, KnownNat (2 ^ n7)

         ) => ToW n01234567 (LittleEndian (a0, a1, a2, a3, a4, a5, a6, a7)) where
    toW (LittleEndian (a0, a1, a2, a3, a4, a5, a6, a7)) = toW a7 >+< toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)
         , n01234567 ~ (n0123456 + n7), n01234567 ~ (n7 + n0123456), KnownNat n01234567, KnownNat (2 ^ n01234567)
         , n012345678 ~ (n01234567 + n8), n012345678 ~ (n8 + n01234567), KnownNat n012345678, KnownNat (2 ^ n012345678)

         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)
         , ToW n7 a7, KnownNat n7, KnownNat (2 ^ n7)
         , ToW n8 a8, KnownNat n8, KnownNat (2 ^ n8)

         ) => ToW n012345678 (LittleEndian (a0, a1, a2, a3, a4, a5, a6, a7, a8)) where
    toW (LittleEndian (a0, a1, a2, a3, a4, a5, a6, a7, a8)) = toW a8 >+< toW a7 >+< toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--------------------
-- FromW, BigEndian
--------------------

instance forall n0 n1 n01 a0 a1. ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)

         , FromW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
         , FromW n1 a1, KnownNat n1, KnownNat (2 ^ n1)

         ) => FromW n01 (BigEndian (a0, a1)) where
    fromW w = BigEndian (fromW a0, fromW a1)
      where
        a0 :: W n0
        a1 :: W n1
        (a0, a1) = split w

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)

--         ) => ToW n012 (BigEndian (a2, a1, a0)) where
--    toW (BigEndian (a2, a1, a0)) = toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)

--         ) => ToW n0123 (BigEndian (a3, a2, a1, a0)) where
--    toW (BigEndian (a3, a2, a1, a0)) = toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)

--         ) => ToW n01234 (BigEndian (a4, a3, a2, a1, a0)) where
--    toW (BigEndian (a4, a3, a2, a1, a0)) = toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
--         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
--         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)

--         ) => ToW n012345 (BigEndian (a5, a4, a3, a2, a1, a0)) where
--    toW (BigEndian (a5, a4, a3, a2, a1, a0)) = toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
--         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
--         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
--         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
--         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)

--         ) => ToW n0123456 (BigEndian (a6, a5, a4, a3, a2, a1, a0)) where
--    toW (BigEndian (a6, a5, a4, a3, a2, a1, a0)) = toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
--         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
--         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)
--         , n01234567 ~ (n0123456 + n7), n01234567 ~ (n7 + n0123456), KnownNat n01234567, KnownNat (2 ^ n01234567)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
--         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
--         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)
--         , ToW n7 a7, KnownNat n7, KnownNat (2 ^ n7)

--         ) => ToW n01234567 (BigEndian (a7, a6, a5, a4, a3, a2, a1, a0)) where
--    toW (BigEndian (a7, a6, a5, a4, a3, a2, a1, a0)) = toW a7 >+< toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
--         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
--         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)
--         , n01234567 ~ (n0123456 + n7), n01234567 ~ (n7 + n0123456), KnownNat n01234567, KnownNat (2 ^ n01234567)
--         , n012345678 ~ (n01234567 + n8), n012345678 ~ (n8 + n01234567), KnownNat n012345678, KnownNat (2 ^ n012345678)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
--         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
--         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)
--         , ToW n7 a7, KnownNat n7, KnownNat (2 ^ n7)
--         , ToW n8 a8, KnownNat n8, KnownNat (2 ^ n8)

--         ) => ToW n012345678 (BigEndian (a8, a7, a6, a5, a4, a3, a2, a1, a0)) where
--    toW (BigEndian (a8, a7, a6, a5, a4, a3, a2, a1, a0)) = toW a8 >+< toW a7 >+< toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

----------------------
---- ToW, LittleEndian
----------------------

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)

--         ) => ToW n01 (LittleEndian (a0, a1)) where
--    toW (LittleEndian (a0, a1)) = toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)

--         ) => ToW n012 (LittleEndian (a0, a1, a2)) where
--    toW (LittleEndian (a0, a1, a2)) = toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)

--         ) => ToW n0123 (LittleEndian (a0, a1, a2, a3)) where
--    toW (LittleEndian (a0, a1, a2, a3)) = toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)

--         ) => ToW n01234 (LittleEndian (a0, a1, a2, a3, a4)) where
--    toW (LittleEndian (a0, a1, a2, a3, a4)) = toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
--         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
--         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)

--         ) => ToW n012345 (LittleEndian (a0, a1, a2, a3, a4, a5)) where
--    toW (LittleEndian (a0, a1, a2, a3, a4, a5)) = toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
--         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
--         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
--         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
--         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)

--         ) => ToW n0123456 (LittleEndian (a0, a1, a2, a3, a4, a5, a6)) where
--    toW (LittleEndian (a0, a1, a2, a3, a4, a5, a6)) = toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
--         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
--         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)
--         , n01234567 ~ (n0123456 + n7), n01234567 ~ (n7 + n0123456), KnownNat n01234567, KnownNat (2 ^ n01234567)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
--         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
--         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)
--         , ToW n7 a7, KnownNat n7, KnownNat (2 ^ n7)

--         ) => ToW n01234567 (LittleEndian (a0, a1, a2, a3, a4, a5, a6, a7)) where
--    toW (LittleEndian (a0, a1, a2, a3, a4, a5, a6, a7)) = toW a7 >+< toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

--instance ( n01 ~ (n0 + n1), n01 ~ (n1 + n0), KnownNat n01, KnownNat (2 ^ n01)
--         , n012 ~ (n01 + n2), n012 ~ (n2 + n01), KnownNat n012, KnownNat (2 ^ n012)
--         , n0123 ~ (n012 + n3), n0123 ~ (n3 + n012), KnownNat n0123, KnownNat (2 ^ n0123)
--         , n01234 ~ (n0123 + n4), n01234 ~ (n4 + n0123), KnownNat n01234, KnownNat (2 ^ n01234)
--         , n012345 ~ (n01234 + n5), n012345 ~ (n5 + n01234), KnownNat n012345, KnownNat (2 ^ n012345)
--         , n0123456 ~ (n012345 + n6), n0123456 ~ (n6 + n012345), KnownNat n0123456, KnownNat (2 ^ n0123456)
--         , n01234567 ~ (n0123456 + n7), n01234567 ~ (n7 + n0123456), KnownNat n01234567, KnownNat (2 ^ n01234567)
--         , n012345678 ~ (n01234567 + n8), n012345678 ~ (n8 + n01234567), KnownNat n012345678, KnownNat (2 ^ n012345678)

--         , ToW n0 a0, KnownNat n0, KnownNat (2 ^ n0)
--         , ToW n1 a1, KnownNat n1, KnownNat (2 ^ n1)
--         , ToW n2 a2, KnownNat n2, KnownNat (2 ^ n2)
--         , ToW n3 a3, KnownNat n3, KnownNat (2 ^ n3)
--         , ToW n4 a4, KnownNat n4, KnownNat (2 ^ n4)
--         , ToW n5 a5, KnownNat n5, KnownNat (2 ^ n5)
--         , ToW n6 a6, KnownNat n6, KnownNat (2 ^ n6)
--         , ToW n7 a7, KnownNat n7, KnownNat (2 ^ n7)
--         , ToW n8 a8, KnownNat n8, KnownNat (2 ^ n8)

--         ) => ToW n012345678 (LittleEndian (a0, a1, a2, a3, a4, a5, a6, a7, a8)) where
--    toW (LittleEndian (a0, a1, a2, a3, a4, a5, a6, a7, a8)) = toW a8 >+< toW a7 >+< toW a6 >+< toW a5 >+< toW a4 >+< toW a3 >+< toW a2 >+< toW a1 >+< toW a0

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
{-# LANGUAGE ConstraintKinds            #-}

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

module Data.Word.N.TupleInstances
    ( AccSums
    ) where

import Data.Word.N.Core
import Data.Word.N.Conversion

import Data.Type.List
import GHC.TypeLits
import GHC.Exts

-- | Convenience constraint synonym.
-- Tuple instances of @'ToW'@ and @'FromW'@ would look a lot uglier without it.
type family AccSums (list :: [Nat]) :: Constraint where
    AccSums '[] = ()
    AccSums (n ': ns) = (Triplet n (Sum ns) (Sum (n ': ns)), AccSums ns)

--------------------
-- ToW
--------------------

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

--------------------
-- FromW
--------------------

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


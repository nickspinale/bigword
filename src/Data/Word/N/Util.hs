{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
-- {-# LANGUAGE IncoherentInstances       #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}

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
    ( Church(..)
    ) where

import Data.Word.N
import Data.Type.List
import Data.Proxy
import GHC.TypeLits

-- | Type synonym for readability. Constructs a the type of functions from
-- the pattern of @'W'@'s specified by 'list' to 'result'.
type Fn list result = Foldr (->) result (Map W list)

-- | Inverse of Fn
type UnFn a = Reverse (UnFnAux '[] a)

type family UnFnAux (acc :: [Nat]) (rest :: *) :: ([Nat], *) where
    UnFnAux acc (W n -> r) = UnFnAux (n ': acc) r
    UnFnAux acc r = '(acc, r)

-- | Acces a church-encoded view of a @'W'@.
class Church list where
    -- | Because @'Fn'@ is not injective, we use a proxy.
    -- This is a temporary solution, until either injective type families come to GHC
    -- (which will happen in the next major release),
    -- or I find a better way of dealing with this problem.
    inspect :: Proxy list -> Fn list result -> W (Sum list) -> result

instance Church '[n] where
    inspect = const ($)

instance ( xyzs ~ Sum (x ': y ': zs)
         , yzs ~ Sum (y ': zs)
         , Triplet yzs x xyzs
         , Church (y ': zs)
         ) => Church (x ': y ': zs) where
    inspect _ f w = inspect (Proxy :: Proxy (y ': zs)) (f high) low 
      where
        high :: W x
        low :: W (y + Sum zs)
        (high, low) = split w

inspect' :: ('(list, result) ~ UnFn (Fn list result), Church list) => Fn list result -> W (Sum list) -> result
inspect' = inspect (Proxy :: list)

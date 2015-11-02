{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

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
-- Provides a church-encoding means of inspecting and constructing bit-vectors.
-- This module is currenty a work in progress.
---------------------------------------------------------

module Data.Word.N.Church
    (
    ) where

import Data.Word.N
import Control.Applicative
import Data.Functor.Identity

import Data.Function
import Data.Proxy
import Data.Type.List
import Data.Type.Function
import Data.Functor.Compose
import GHC.TypeLits
import GHC.Exts (Constraint)

-- | Type synonym for readability. Constructs a the type of functions from
-- the pattern of @'W'@'s specified by 'list' to 'result'.
type Fn f list result = Foldr (->) result (Map f list)

-- | @'Fn'@ is not injective, but @'Fun'@ is, which is necessary for type decidability.
newtype Fun (f :: k -> *) (list :: [k]) (result :: *) = Fun { getFun :: Fn f list result }

newtype Ws (list :: [Nat]) = Ws { getWs :: W (Sum list) }

class Arity (list :: [k]) where
    accum :: (forall x xs. fs (x ': xs) -> f x -> fs xs) -> (fs '[] -> result) -> fs list -> Fun f list result
    apply :: (forall x xs. fs (x ': xs) -> (f x, fs xs)) -> fs list -> Fun f list result -> result

instance Arity '[] where
    accum f g t = Fun (g t)
    apply f t h = getFun h

instance Arity xs => Arity (x ': xs) where
    accum f g t = Fun $ \a -> getFun (accum f g (f t a))
    apply f t h = case f t of (a, u) -> apply f u (Fun (getFun h a))

type family AllSatisfy (c :: k -> Constraint) (list :: [k]) :: Constraint where
    AllSatisfy c '[] = ()
    AllSatisfy c (x ': xs) = (c x, AllSatisfy c xs)

class Vector (c :: k -> Constraint) (f :: k -> *) (fs :: [k] -> *) where
    hcons :: (c x, AllSatisfy c xs) => f x -> fs xs -> fs (x ': xs)
    construct :: Arity list => Fun f list (fs list)
    inspect :: Arity list => fs list -> Fun f list result -> result

instance Vector KnownNat W Ws where
    hcons x (Ws xs) = Ws (x >+< xs)

-- | Class for nonempty type-level lists of @'Nat'@'s.
-- This is the core of the heterogeneous-church-encoded-vector-like interface.
-- class Church (list :: [Nat]) where
--     -- | Given components, return their concatenation.
--     construct :: Fun list (Wof list)
--     -- | Operate on a bit-vector component-wise, where the size of its components are determined by `list`.
--     inspect :: Fun list result -> Wof list -> result

-- instance Church '[n] where
--     construct = FunCons FunNil
--     inspect = ((.).(.)) getFunNil getFunCons

-- -- | Constraint synonym for readablility
-- type ListSum (n :: Nat) (ns :: [Nat]) = Triplet n (Sum ns) (n + Sum ns)

-- instance ( ListSum m (n ': ns)
--          , Functor (Fun ns)
--          , Church (n ': ns)
--          ) => Church (m ': n ': ns) where

--     construct = FunCons $ (`fmap` construct) . (>+<)
--     -- EQUIVALENT:
--     -- construct = FunCons f
--     --   where
--     --     f :: W m -> Fun (n ': ns) (Wof (m ': n ': ns))
--     --     f h = fmap (h >+<) construct

--     inspect = (. split) . (uncurry . (inspect .) . getFunCons)
--     -- EQUIVALENT:
--     -- inspect f = uncurry (inspect . getFunCons f) . split
--     -- inspect f w = let (head, low) = split w
--     --               in inspect (getFunCons f head) low

-----------------
-- EVEN MORE EXPERIMENTAL
-----------------

-- newtype View m n = View { getView :: W (m + n) }

-- newtype Ws (list :: [Nat])

-- class List (list :: [Nat]) where
--     wut :: (forall x xs. f (W x) -> f (Ws xs

-- class Arity (a :: k) (zero :: k) (op :: k -> t k -> t k) where
--     f :: ( forall (x :: k) (xs :: t k)
--          . t (op x xs) -> (t x, t xs)
--          )
--       -> t (Foldr op zero a)
--       -> (Foldr t list) r -> r

-- class NonEmpty (ns :: [Nat]) where

-- inspect :: (Applicative f, NonEmpty ns) => (forall n. => (W n -> f (W n))) -> Ws


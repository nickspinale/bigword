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

-- newtype View m n = View { getView :: W (m + n) }

type Fn list result = Foldr (->) result list

-- newtype Ws (list :: [Nat])

-- class List (list :: [Nat]) where
--     wut :: (forall x xs. f (W x) -> f (Ws xs

-- class Arity (a :: k) (zero :: k) (op :: k -> t k -> t k) where
--     f :: ( forall (x :: k) (xs :: t k)
--          . t (op x xs) -> (t x, t xs)
--          )
--       -> t (Foldr op zero a)
--       -> (Foldr t list) r -> r

class NonEmpty (ns :: [Nat]) where

-- inspect :: (Applicative f, NonEmpty ns) => (forall n. => (W n -> f (W n))) -> Ws

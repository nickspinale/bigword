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
-- type Fn list result = Foldr (->) result (Map W list)

data family Fun (list :: [Nat]) (result :: *) :: *
newtype instance Fun '[] result = FunNil { getFunNil :: result }
newtype instance Fun (x ': xs) result = FunCons { getFunCons :: W x -> Fun xs result }

instance Functor (Fun '[]) where
    fmap f (FunNil result) = FunNil (f result)

instance Functor (Fun xs) => Functor (Fun (x ': xs)) where
    fmap f (FunCons g) = FunCons (fmap f . g)

-- type ListSum n ns = Triplet n (Sum ns) (Sum (n ': ns))

type Wof list = W (Sum list)
-- type Wof = W :.: Sum

class Church (list :: [Nat]) where
    construct :: Fun list (Wof list)
    inspect :: Fun list result -> Wof list -> result

instance Church '[n] where
    construct = FunCons FunNil
    inspect = ((.).(.)) getFunNil getFunCons

type family AllKnown (list :: [Nat]) :: Constraint where
    AllKnown '[] = ()
    AllKnown (x ': xs) = ( BothKnown x
                         , BothKnown (x + Sum xs)
                         , AllKnown xs
                         )

instance ( AllKnown (m ': n ': ns)
         , Functor (Fun ns)
         , Church (n ': ns)
         ) => Church (m ': n ': ns) where

    construct = FunCons $ (`fmap` construct) . (>+<)
    -- construct = FunCons f
    --   where
    --     f :: W m -> Fun (n ': ns) (Wof (m ': n ': ns))
    --     f h = fmap (h >+<) construct

    inspect = (. split) . (uncurry . (inspect .) . getFunCons)
    -- inspect f = uncurry (inspect . getFunCons f) . split
    -- inspect f w = let (head, low) = split w
    --               in inspect (getFunCons f head) low

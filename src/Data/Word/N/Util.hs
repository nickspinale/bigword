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
import Data.Functor.Compose
import GHC.TypeLits

-- | Type synonym for readability. Constructs a the type of functions from
-- the pattern of @'W'@'s specified by 'list' to 'result'.
type Fn list result = Foldr (->) result (Map W list)

-- data family Fun (list :: [Nat]) (result :: *) :: *
-- newtype instance Fun '[] result = FunNil { getFunNil :: result }
-- newtype instance Fun (x ': xs) result = FunCons { getFunCons :: W x -> Fun xs result }

-- instance Functor (Fun '[]) where
--     fmap f (FunNil result) = FunNil (f result)

-- instance Functor (Fun xs) => Functor (Fun (x ': xs)) where
--     fmap f (FunCons g) = FunCons (fmap f . g)

newtype Fun list result = Fun { unFun :: Fn list result }

type ListSum n ns = Triplet n (Sum ns) (n + Sum ns)

class Church (head :: Nat) (tail :: [Nat]) where
    construct :: Fun (head ': tail) (W (head + Sum tail))
    inspect :: Fun (head ': tail) result -> W (head + Sum tail) -> result

instance Church head '[] where
    construct = Fun id
    -- inspect _ _ f w = unFun (f w)

instance (ListSum head (n ': ns), Church n ns) => Church head (n ': ns) where

    -- construct _ _ head = Fun (unFun . f)
    --   where
    --     f :: W n -> Fun ns (W (head + Sum (n ': ns)))
    --     f n = Fun $ (head >+<) . (unFun $ construct (Proxy :: Proxy n) (Proxy :: Proxy ns) undefined)
--     construct _ _ head = undefined -- Fun (\x -> f (unFun)
--       where
--         f :: W n -> Fun ns (W (head + Sum (n ': ns)))
--         f n = Fun ((head >+<) . unFun x)
--           where
--             x :: Fun ns (W (Sum (n ': ns)))
--             x = undefined
            -- x = construct (Proxy :: Proxy n) (Proxy :: Proxy ns) n
            -- x = construct (Proxy :: Proxy n) (Proxy :: Proxy ns) n
    inspect f w =
        let (head, low) = split w
        in inspect (Fun (unFun f head) :: Fun (n ': ns) result) low

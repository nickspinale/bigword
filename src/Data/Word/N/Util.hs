{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
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
    ( Church(..)
    ) where

import Data.Word.N

import Data.Function
import Data.Proxy
import Data.Type.List
import Data.Functor.Compose
import GHC.TypeLits

-- | Type synonym for readability. Constructs a the type of functions from
-- the pattern of @'W'@'s specified by 'list' to 'result'.
-- type Fn list result = Foldr (->) result (Map W list)

class Functor fn => Church (head ::  Nat  )
                           (tail :: [Nat] )
                           (sum  ::  Nat  )
                           (fn   :: * -> *) | fn -> head
                                            , fn -> tail
                                            , fn -> sum where

    inspect :: Proxy (head ': tail) -> fn result -> W sum -> result
    construct :: Proxy (head ': tail) -> fn (W sum)
    -- construct :: Proxy list -> Fn list (W (Sum list))

instance Church head '[] head ((->) (W head)) where
    inspect = flip const
    construct = flip const

instance ( Triplet head sum sum'
         , Church x xs sum fn
         ) => Church head (x ': xs) sum' (Compose ((->) (W head)) fn) where

    inspect _ f w = inspect (Proxy :: Proxy (x ': xs)) (getCompose f high) low 
      where
        high :: W head
        low :: W sum
        (high, low) = split w

    construct _ = Compose $ \high -> fmap ((high :: W head) >+<) (construct (Proxy :: Proxy (x ': xs)))

-- instance ( Triplet Church x xs => Church head (x ': xs) where
--     inspect _ f w = inspect (Proxy :: Proxy '(x, xs)) (f high) low 
--       where
--         (high, low) = split w

-- class Church head tail where
--     inspect :: Proxy '(head, tail) -> (W head -> Fn tail result) -> W (head + Sum tail) -> result
--     -- construct :: Proxy list -> Fn list (W (Sum list))

-- instance Church head '[] where
--     inspect _ f w = f w
--     -- construct = flip const

-- instance ( Triplet Church x xs => Church head (x ': xs) where
--     inspect _ f w = inspect (Proxy :: Proxy '(x, xs)) (f high) low 
--       where
--         (high, low) = split w

-- instance Church (y ': zs) => Church (x ': y ': zs) where
-- instance ( Triplet x (Sum (y ': zs)) (Sum (x ': y ': zs))
--          , Church (y ': zs)
--          ) => Church (x ': y ': zs) where
-- instance ( Triplet x (Sum 
--          , Church (y ': zs)
--          ) => Church head (x ': xs) where

--     inspect _ f w = inspect (Proxy :: Proxy (y ': zs)) (f high) low 
--       where
--         (high, low) = split w

--     construct _ = \high -> ((high :: W x) >+<) . (construct (Proxy :: Proxy (y ': zs)) :: Fn (y ': zs) (W (Sum (y ': zs))))

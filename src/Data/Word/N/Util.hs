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

type family Flatten a where
    Flatten (Identity b) = b
    Flatten (Compose c d) = c (Flatten d)

-- instance Flatten Identity 

class Functor f => Church (list :: [Nat] )
                          (sum  ::  Nat  )
                          (f    :: * -> *) | list -> sum, list -> f where

    inspect :: Proxy list -> f result -> W sum -> result
    construct :: Triplet n sum newSum => Proxy list -> W n -> f (W newSum)
    -- construct :: Proxy (head ': tail) -> fn (W sum)
    -- construct :: Proxy list -> Fn list (W (Sum list))

instance Church '[] 0 Identity where
    inspect _ f _ = runIdentity f
    construct _ = Identity

instance ( Triplet x sum sum'
         , Church list sum f
         ) => Church (x ': list) sum' (Compose ((->) (W x)) f) where
    inspect _ f w = inspect (Proxy :: Proxy list) (getCompose f high) low
      where
        high :: W x
        low :: W sum
        (high, low) = split w
    construct _ high = Compose $ (fmap . fmap) (high >+<) (construct (Proxy :: Proxy list))

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
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE PolyKinds                  #-}
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

import Control.Applicative
import Data.Word.N
-- import Data.Type.Function
import Data.Proxy
import Data.Traversable
import Data.Type.Equality
import Data.Type.Bool
import GHC.TypeLits

-- type Fn xs res = Foldr (->) res xs

-- churchify :: forall (n :: Nat) (list :: [Nat]) (result :: *).
--              (n ~ Sum list) => W n -> Fn (Map W list) result -> result
-- churchify = undefined

-- class Church (list :: [Nat]) where
--     type View list result :: *
--     inspect :: View list result -> W (Sum list) -> result

-- instance Church (n ': '[]) where
--     type View (n ': '[]) result = W n -> result
--     inspect = id

-- instance Church (m ': os) => Church (n ': m ': os) where
--     type View (n ': m ': os) result = W n -> View (m ': os) result
--     inspect v w = inspect (v high) low
--       where
--         (high, low) = split w



-- class Church (list :: [Nat]) where
--     type Fun list result :: *
--     type Sum list :: Nat
--     inspect :: (m ~ Sum list) => Fun list result -> W m -> result

-- instance Church '[] where
--     type Fun '[] result = result
--     type Sum '[] = 0
--     inspect = const

-- -- instance (View (n ': ns) result ~ View (n ': ns) result, Church ns) => Church (n ': ns) where
-- instance Church ns => Church (n ': ns) where
--     type Fun (n ': ns) result = W n -> Fun ns result
--     type Sum (n ': ns) = n + Sum ns
--     inspect v w = inspect (v high) low
--       where
--         high :: W n
--         low :: W (Sum ns)
--         (high, low) = split w

class Church f sum result | f -> sum, f -> result where
    inspect :: W sum -> f -> result

instance Church (W n -> result) n result where
    inspect = flip ($)

instance ( Triplet m n o
         , Church f m result
         ) => Church (W n -> f) o result where
    inspect w f = inspect low $ f high
      where
        high :: W n
        low :: W m
        (high, low) = split w

subj :: W 2
subj = 2

look :: W 1 -> W 1 -> String
look x y = show x ++ show y

go :: String
go = inspect subj look

-- instance Church '[] 0 (W 0 -> a) where
--     inspect _ f = f

-- type family Id (a :: *) :: * where
--     Id a = a

-- -- -- instance (View (n ': ns) result ~ View (n ': ns) result, Church ns) => Church (n ': ns) where
-- instance ( Church ns oldSum oldFunction
--          , sum ~ (n + oldSum)
--          , function ~ (W n -> oldFunction)
--          ) => Church (n ': ns) sum function where
--     inspect = undefined

-- class Folding (list :: [a]) where
--     fold :: forall (f :: a -> *) comb. (forall n. f n) -> (forall m n. f m -> f n -> f (comb m n)) -> Fn (Map f list) (f (Foldr1 comb list))

-- instance Folding ('[x] :: [a]) where
--     fold sing _ = sing

-- class KnownList (a :: [b]) where
    -- knowList :: b -> (

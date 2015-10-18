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

-- inspect :: forall n ns. Church ns => Proxy

-- | Type synonym for readability. Constructs a the type of functions from
-- the pattern of @'W'@'s specified by 'list' to 'result'.
type Fn list result = Foldr (->) result (Map W list)

newtype Fun list result = Fun { unFun :: Fn list result }

type ListSum n ns = Triplet n (Sum ns) (n + Sum ns)

-- type family Flatten a where
--     Flatten (Identity b) = b
--     Flatten (Compose c d) = c (Flatten d)

-- instance Flatten Identity 

class ListSum head tail => Church (head :: Nat) (tail :: [Nat]) where
    construct :: Proxy head -> Proxy tail -> W head -> Fun tail (W (head + Sum tail))
    inspect :: Proxy head -> Proxy tail -> (W head -> Fun tail result) -> W (head + Sum tail) -> result

instance ListSum head '[] => Church head '[] where
    construct _ _ = unFun
    inspect _ _ = unFun
--     -- construct _ high = high
--     -- inspect _ f w = f w

-- -- type family UnProxy a where
-- --     UnProxy a = W a

-- instance (ListSum head (n ': ns), Church n ns) => Church head (n ': ns) where
--     -- construct :: ListSum high (n ': ns) => Proxy high
--     --                                     -> Proxy (n ': ns)
--     --                                     -> W high
--     --                                     -> Fn (n ': ns) (W (high + Sum (n ': ns)))
--     -- construct proxy _ high = ((high :: W high) >+<) . construct (Proxy :: Proxy n) (Proxy :: Proxy ns)
--     construct _ _ head = ((head :: W head) >+<)
--                        . (construct (Proxy :: Proxy n) (Proxy :: Proxy ns))

--     inspect _ _ f w =
--         let (head, low) = split w
--         in inspect (Proxy :: Proxy n) (Proxy :: Proxy ns) (f head) low
-- class ListSum head tail => Church (head :: Nat) (tail :: [Nat]) where

--     construct :: Proxy head -> Proxy tail -> W head -> Fn tail (W (head + Sum tail))

--     inspect :: Proxy head -> Proxy tail -> (W head -> Fn tail result) -> W (head + Sum tail) -> result

-- instance ListSum head '[] => Church head '[] where
--     construct _ _ = id
--     inspect _ _ = id
--     -- construct _ high = high
--     -- inspect _ f w = f w

-- -- type family UnProxy a where
-- --     UnProxy a = W a

-- instance (ListSum high (n ': ns), Church n ns) => Church high (n ': ns) where
--     -- construct :: ListSum high (n ': ns) => Proxy high
--     --                                     -> Proxy (n ': ns)
--     --                                     -> W high
--     --                                     -> Fn (n ': ns) (W (high + Sum (n ': ns)))
--     -- construct proxy _ high = ((high :: W high) >+<) . construct (Proxy :: Proxy n) (Proxy :: Proxy ns)
--     construct _ _ high = ((high :: W high) >+<)
--                        . (construct (Proxy :: Proxy n) (Proxy :: Proxy ns)
--                             :: W n -> Fn (n ': ns) (W (n + Sum ns)))

--     inspect _ _ f w =
--         let (high, low) = split w
--         in inspect (Proxy :: Proxy n) (Proxy :: Proxy ns) (f high) low

-- instance ( Triplet x sum sum'
--          , Church list sum f
--          ) => Church (x ': list) sum' (Compose ((->) (W x)) f) where
--     inspect _ f w = inspect (Proxy :: Proxy list) (getCompose f high) low
--       where
--         high :: W x
--         low :: W sum
--         (high, low) = split w
--     construct _ high = Compose $ (fmap . fmap) (high >+<) (construct (Proxy :: Proxy list))

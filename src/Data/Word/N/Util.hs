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

import Control.Applicative
import Data.Word.N
import Data.Type.List
import Data.Proxy
import Data.Traversable
import Data.Type.Equality
import Data.Type.Bool
import GHC.TypeLits

type Fn list result = Foldr (->) result list

class Church list sum | list -> sum where
    inspect :: Proxy list -> Fn (Map W list) result -> W sum -> result

instance Church '[n] n where
    inspect = const ($)

instance (Triplet nos m mnos, Church (n ': os) nos) => Church (m ': n ': os) mnos where
    inspect _ f w = inspect (Proxy :: Proxy (n ': os)) (f high) low 
      where
        high :: W m
        low :: W nos
        (high, low) = split w

subj :: W 2
subj = 2

look :: W 1 -> W 1 -> String
look x y = show x ++ show y

go :: String
go = inspect (Proxy :: Proxy '[1, 1]) look subj 

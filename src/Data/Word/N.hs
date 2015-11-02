---------------------------------------------------------
-- |
-- Module      : Data.Word.N.Core
-- Copyright   : (c) 2015 Nick Spinale
-- License     : MIT
--
-- Maintainer  : Nick Spinale <spinalen@carleton.edu>
-- Stability   : provisional
-- Portability : portable
--
-- Fixed size bit vectors using type-level naturals.
---------------------------------------------------------

module Data.Word.N
    (
    -- * The 'W' newtype
      W

    -- * Operations
    , (>+<)
    , split

    -- * Convenience
    , BothKnown
    , Triplet
    , AccSums

    -- * Conversion Classes
    , ToW(..)
    , FromW(..)
    ) where

import Data.Word.N.Conversion
import Data.Word.N.Core

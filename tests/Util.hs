{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Test where

import Data.Word.N
import Numeric.Mod
import Data.Bits

showBits :: (1 :|: n) => W n -> String
showBits = disassembleR (show :: W 1 -> String)

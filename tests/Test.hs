{-# LANGUAGE DataKinds #-}

module Test where

import Data.Word.N

showBits :: W n -> String
showBits = disassembleR (show :: W 1 -> String)

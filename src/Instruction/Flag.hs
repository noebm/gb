module Instruction.Flag where

import Control.Lens
import Data.Word

import MonadEmulator.Operations

data Flag = FlagZ | FlagC | FlagNZ | FlagNC
  deriving (Eq)

instance Show Flag where
  show FlagZ  = "Z"
  show FlagNZ = "NZ"
  show FlagC  = "C"
  show FlagNC = "NC"

{-# INLINE flag #-}
flag :: Word8 -> Flag
flag w = case w of
  0 -> FlagNZ
  1 -> FlagZ
  2 -> FlagNC
  3 -> FlagC
  _ -> error "flag: invalid argument"

{-# INLINE getFlag #-}
getFlag :: MonadEmulator m => Flag -> m Bool
getFlag FlagC = view flagC <$> loadReg F
getFlag FlagZ = view flagZ <$> loadReg F
getFlag FlagNC = views flagC not <$> loadReg F
getFlag FlagNZ = views flagZ not <$> loadReg F

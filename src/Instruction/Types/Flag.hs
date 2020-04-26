module Instruction.Types.Flag where

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

{-# INLINE parseFlag #-}
parseFlag :: Word8 -> Flag
parseFlag w = case w of
  0 -> FlagNZ
  1 -> FlagZ
  2 -> FlagNC
  3 -> FlagC
  _ -> error "flag: invalid argument"

{-# INLINE evalFlag #-}
evalFlag :: MonadEmulator m => Flag -> m Bool
evalFlag FlagC = view flagC <$> loadReg F
evalFlag FlagZ = view flagZ <$> loadReg F
evalFlag FlagNC = views flagC not <$> loadReg F
evalFlag FlagNZ = views flagZ not <$> loadReg F

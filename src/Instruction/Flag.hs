module Instruction.Flag where

import Control.Lens
import Data.Word

import MonadEmulator

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
getFlag :: MonadEmulator m => Maybe Flag -> m Bool
getFlag Nothing = return True
getFlag (Just FlagC) = view flagC <$> loadReg F
getFlag (Just FlagZ) = view flagZ <$> loadReg F
getFlag (Just FlagNC) = views flagC not <$> loadReg F
getFlag (Just FlagNZ) = views flagZ not <$> loadReg F

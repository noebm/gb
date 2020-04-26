module Instruction.Types.Writable where

import MonadEmulator.Operations

import Instruction.Types.Address

import Data.Word
import Text.Printf

data Writable8 = WriteReg8 !Reg8 | WriteAddr8 !Addr

instance Show Writable8 where
  show arg = case arg of
    WriteReg8 r -> show r
    WriteAddr8 addr -> show addr

{-# INLINE write8 #-}
write8 :: MonadEmulator m => Writable8 -> Word8 -> m ()
write8 (WriteReg8 r)     = storeReg r
write8 (WriteAddr8 addr) = \b -> (`storeAddr` b) =<< getAddress addr

data Writable16 = WriteReg16 !Reg16
           | WriteSP
           | WriteImmAddr16 !Word16

instance Show Writable16 where
  show arg = case arg of
    WriteReg16 r -> show r
    WriteSP -> "SP"
    WriteImmAddr16 addr -> printf "(0x%04x)" addr

{-# INLINE write16 #-}
write16 :: MonadEmulator m => Writable16 -> Word16 -> m ()
write16 arg = case arg of
  WriteReg16 r   -> storeReg16 r
  WriteSP        -> storeSP
  WriteImmAddr16 addr -> storeAddr16 addr

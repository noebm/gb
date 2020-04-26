module Instruction.Types.Readable where

import MonadEmulator.Operations
import Instruction.Types.Address

import Data.Word
import Text.Printf

data Readable8 = ReadReg8 !Reg8 | ReadImm8 !Word8 | ReadAddr8 !Addr

instance Show Readable8 where
  show arg = case arg of
    ReadReg8 r -> show r
    ReadImm8 dat -> printf "0x%02x" dat
    ReadAddr8 addr -> show addr

{-# INLINE read8 #-}
read8 :: MonadEmulator m => Readable8 -> m Word8
read8 (ReadReg8 r)     = loadReg r
read8 (ReadAddr8 addr) = loadAddr =<< getAddress addr
read8 (ReadImm8 dat)   = return dat

data Readable16 = ReadReg16 !Reg16
         | ReadSP
         | ReadImm16 !Word16
         | ReadImmAddr16 !Word16
         deriving (Eq)

instance Show Readable16 where
  show arg = case arg of
    ReadReg16 r -> show r
    ReadSP -> "SP"
    ReadImm16 dat -> printf "0x%04x" dat
    ReadImmAddr16 addr -> printf "(0x%04x)" addr

{-# INLINE read16 #-}
read16 :: MonadEmulator m => Readable16 -> m Word16
read16 arg = case arg of
  ReadSP        -> loadSP
  ReadReg16 r   -> loadReg16 r
  ReadImm16 dat -> return dat
  ReadImmAddr16 addr -> loadAddr16 addr

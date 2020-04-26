module Instruction.Types.Address where

import Control.Lens

import MonadEmulator.Operations

import Data.Word

import Text.Printf

data Addr = AddrBC | AddrDE | AddrHL | AddrHLi | AddrHLd | AddrDirect !Word16 | ZeroPage !Word8 | ZeroPageC

instance Show Addr where
  show arg = printf "(%s)" $ case arg of
    AddrBC -> "BC"
    AddrDE -> "DE"
    AddrHL -> "HL"
    AddrHLi -> "HLi"
    AddrHLd -> "HLd"
    AddrDirect addr -> printf "0x%04x" addr
    ZeroPage addr -> printf "0xff00 + 0x%02x" addr
    ZeroPageC     -> printf "0xff00 + C"

{-# INLINE addrFF #-}
addrFF :: Word8 -> Word16
addrFF k = (0xFF , k) ^. word16

{-# INLINE getAddress #-}
getAddress :: MonadEmulator m => Addr -> m Word16
getAddress AddrBC = loadReg16 BC
getAddress AddrDE = loadReg16 DE
getAddress AddrHL = loadReg16 HL
getAddress AddrHLi = do
  hl <- loadReg16 HL
  storeReg16 HL (hl + 1)
  return hl
getAddress AddrHLd = do
  hl <- loadReg16 HL
  storeReg16 HL (hl - 1)
  return hl
getAddress (AddrDirect addr) = return $! addr
getAddress (ZeroPage addr  ) = return $! addrFF addr
getAddress ZeroPageC  = addrFF <$> loadReg C

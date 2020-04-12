module Instruction.InOut where

import Control.Lens
import Data.Word

import MonadEmulator.Operations

data In16 = InReg16 Reg16
         | InSP
         | InImm16
         | InImmAddr16
         deriving (Eq)

data Out16 = OutReg16 Reg16
           | OutSP
           | OutImmAddr16

{-# INLINE out16ToIn16 #-}
out16ToIn16 :: Out16 -> In16
out16ToIn16 (OutReg16 r) = InReg16 r
out16ToIn16 OutSP = InSP
out16ToIn16 OutImmAddr16 = InImmAddr16

instance Show In16 where
  show arg = case arg of
    InReg16 r -> show r
    InSP -> "SP"
    InImm16 -> "d16"
    InImmAddr16   -> "(a16)"

instance Show Out16 where
  show arg = case arg of
    OutReg16 r -> show r
    OutSP -> "SP"
    OutImmAddr16 -> "(a16)"

data In8 = InReg8 Reg8 | InImm8 | InAddr8 Addr
  deriving Show

data Out8 = OutReg8 Reg8 | OutAddr8 Addr
  deriving Show

data Addr = AddrBC | AddrDE | AddrHL | AddrHLi | AddrHLd | AddrDirect | ZeroPage | ZeroPageC
  deriving Show

-- every output is also in input
{-# INLINE outToIn #-}
outToIn :: Out8 -> In8
outToIn (OutReg8 r) = InReg8 r
outToIn (OutAddr8 addr) = InAddr8 addr

{-# INLINE getIn16 #-}
getIn16 :: MonadEmulator m => In16 -> m Word16
getIn16 arg = case arg of
  InSP        -> loadSP
  InReg16 r   -> loadReg16 r
  InImm16     -> word
  InImmAddr16 -> loadAddr16 =<< word

{-# INLINE setOut16 #-}
setOut16 :: MonadEmulator m => Out16 -> Word16 -> m ()
setOut16 arg = case arg of
  OutReg16 r   -> storeReg16 r
  OutSP        -> storeSP
  OutImmAddr16 -> \w -> (`storeAddr16` w) =<< word

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
getAddress AddrDirect = word
getAddress ZeroPage   = addrFF <$> byte
getAddress ZeroPageC  = addrFF <$> loadReg C

{-# INLINE getIn8 #-}
getIn8 :: MonadEmulator m => In8 -> m Word8
getIn8 (InReg8 r)     = loadReg r
getIn8 (InAddr8 addr) = loadAddr =<< getAddress addr
getIn8 InImm8         = byte

{-# INLINE setOut8 #-}
setOut8 :: MonadEmulator m => Out8 -> Word8 -> m ()
setOut8 (OutReg8 r)     = storeReg r
setOut8 (OutAddr8 addr) = \b -> (`storeAddr` b) =<< getAddress addr

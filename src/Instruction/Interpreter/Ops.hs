module Instruction.Interpreter.Ops where

import Data.Bits
import Data.Word

import Control.Lens hiding (op)
import Data.Bits.Lens

import MonadEmulator.Operations

import Instruction.Parser (readable8)
import Instruction.Types.Readable
import Instruction.Types.Writable

{-# INLINE modifyFlags #-}
modifyFlags :: MonadEmulator m => (Word8 -> Word8) -> m ()
modifyFlags g = do
  flags <- loadReg F
  storeReg F $ g flags

{-# INLINE daa #-}
daa :: MonadEmulator m => m ()
daa = do
  f <- loadReg F
  v <- loadReg A
  let vcorr'
        | f ^. flagN
        = (if f ^. flagC then 0x60 else 0x00)
        + (if f ^. flagH then 0x06 else 0x00)
        | otherwise
        = (if f ^. flagC || v > 0x99              then 0x60 else 0x00)
        + (if (f ^. flagH) || (v .&. 0x0f) > 0x09 then 0x06 else 0x00)
  let v' = if f ^. flagN then v - vcorr' else v + vcorr'
  storeReg A v'
  storeReg F $ f
    & flagH .~ False
    & flagC .~ (f ^. flagC || (not (f ^. flagN) && v > 0x99))
    & flagZ .~ (v' == 0)

{-# INLINE add #-}
add :: Word8 -> Word8 -> Bool -> (Word8 , Word8)
add a v c =
  (s , 0x00 & flagZ .~ (s == 0) & flagH .~ (carry_info `testBit` 4) & flagC .~ (carry_info `testBit` 8))
  where
    v' = fromIntegral v
    a' = fromIntegral a
    s' = a' + v' :: Int
    s'' = s' + fromEnum c

    carry_info = let f = xor (a' `xor` v') in (f s' .|. f s'')
    s = fromIntegral s''

{-# INLINE sub #-}
sub :: Word8 -> Word8 -> Bool -> (Word8 , Word8)
sub a v c =
  (s , 0x40 & flagZ .~ (s == 0) & flagH .~ (carry_info `testBit` 4) & flagC .~ (carry_info `testBit` 8))
  where
    v' = fromIntegral v
    a' = fromIntegral a
    s' = a' - v' :: Int
    s'' = s' - fromEnum c

    carry_info = let f = xor (a' `xor` v') in (f s' .|. f s'')
    s = fromIntegral s''

{-# INLINE arith #-}
arith :: MonadEmulator m
      => (Word8 -> Word8 -> Bool -> (Word8 , Word8))
      -> Readable8
      -> Bool
      -> m ()
arith fun arg useCarry = do
  k <- read8 arg
  a <- loadReg A
  cf <- if useCarry then view flagC <$> loadReg F else return False
  let (a' , f) = fun a k cf
  storeReg A a'
  storeReg F f

{-# INLINE logicOp #-}
logicOp :: MonadEmulator m => (Word8 -> Word8 -> Word8) -> (Word8 -> Word8) -> Readable8 -> m ()
logicOp op flag arg = do
  v <- read8 arg
  a <- loadReg A
  let a' = op v a
  storeReg A a'
  storeReg F $ flag a'

{-# INLINE bitShiftCarryOp #-}
bitShiftCarryOp :: MonadEmulator m
         => (Word8 -> Bool -> (Word8, Bool))
         -> (Word8 -> Bool -> Word8)
         -> Writable8 -> m ()
bitShiftCarryOp op flag arg = do
  c <- view flagC <$> loadReg F
  bitShiftOp (\v -> op v c) flag arg

{-# INLINE bitShiftOp #-}
bitShiftOp :: MonadEmulator m
              => (Word8 -> (Word8 , Bool))
              -> (Word8 -> Bool -> Word8)
              -> Writable8 -> m ()
bitShiftOp op flag arg = do
  v <- read8 (readable8 arg)
  let (v' , c') = op v
  write8 arg v'
  storeReg F (flag v' c')

{-# INLINE rotateLeft #-}
rotateLeft :: Word8 -> Bool -> (Word8, Bool)
rotateLeft v c =
  let v' = v `rotateL` 1
      c' = v' `testBit` 0
  in (v' & bitAt 0 .~ c, c')

{-# INLINE rotateRight #-}
rotateRight :: Word8 -> Bool -> (Word8, Bool)
rotateRight v c =
  let v' = v `rotateR` 1
      c' = v' `testBit` 7
  in (v' & bitAt 7 .~ c, c')

{-# INLINE rotateLeftCarry #-}
rotateLeftCarry :: Word8 -> (Word8, Bool)
rotateLeftCarry v = rotateLeft v (v `testBit` 7)

{-# INLINE rotateRightCarry #-}
rotateRightCarry :: Word8 -> (Word8, Bool)
rotateRightCarry v = rotateRight v (v `testBit` 0)

{-# INLINE shiftLeftArithmetic #-}
shiftLeftArithmetic :: Word8 -> (Word8, Bool)
shiftLeftArithmetic v =
  let v' = v `shiftL` 1
      c' = v `testBit` 7
  in (v', c')

{-# INLINE shiftRightArithmetic #-}
shiftRightArithmetic :: Word8 -> (Word8, Bool)
shiftRightArithmetic v =
  let v' = v `shiftR` 1
      c' = v `testBit` 0
  in (v' & bitAt 7 .~ (v `testBit` 7), c')

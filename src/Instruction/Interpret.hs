module Instruction.Interpret where

import Data.Word
import Data.Bits

import Control.Lens hiding (op, to, from)
import Control.Monad

import Instruction.Instruction
import Instruction.Time
import Instruction.InOut
import Instruction.Ops
import Instruction.Flag

import GB
import MonadEmulator

{-# SPECIALISE interpretM :: Instruction -> GB IO (Word , StepInfo) #-}
interpretM :: (HardwareMonad m, MonadEmulator m) => Instruction -> m (Word , StepInfo)
interpretM instr@(Instruction _ t op) = case op of
  NOP -> (,) (getTime True t) <$> prefetch

  LD from to -> do
    setOut8 to =<< getIn8 from
    (,) (getTime True t) <$> prefetch

  LD16 from to -> do
    setOut16 to =<< getIn16 from
    (,) (getTime True t) <$> prefetch

  LD16_SP_HL -> do
    sp <- loadSP
    r <- sbyte
    let v = addRelative sp r
    storeReg16 HL v
    storeReg F $ 0x00
      & flagC .~ ((v .&. 0xFF) < (sp .&. 0xFF))
      & flagH .~ ((v .&. 0x0F) < (sp .&. 0x0F))
    (,) (getTime True t) <$> prefetch

  AND arg -> do
    logicOp (.&.) (\a' -> 0x20 & flagZ .~ (a' == 0)) arg
    (,) (getTime True t) <$> prefetch

  OR arg -> do
    logicOp (.|.) (\a' -> 0x00 & flagZ .~ (a' == 0)) arg
    (,) (getTime True t) <$> prefetch

  XOR arg -> do
    logicOp xor (\a' -> 0x00 & flagZ .~ (a' == 0)) arg
    (,) (getTime True t) <$> prefetch

  {- 0xCB instructions and specialization for A -}
  BIT y arg -> do
    v <- getIn8 arg
    modifyFlags $ \f -> f
      & flagZ .~ not (v `testBit` fromIntegral y)
      & flagN .~ False
      & flagH .~ True
    (,) (getTime True t) <$> prefetch

  SWAP arg -> do
    x <- getIn8 (outToIn arg)
    let x' = ((x `shiftL` 4) .&. 0xF0) .|. ((x `shiftR` 4) .&. 0x0F)
    setOut8 arg x'
    modifyFlags $ \_ -> 0x00 & flagZ .~ (x' == 0)
    (,) (getTime True t) <$> prefetch

  RES bidx arg -> do
    setOut8 arg . (`clearBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    (,) (getTime True t) <$> prefetch

  SET bidx arg -> do
    setOut8 arg . (`setBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    (,) (getTime True t) <$> prefetch

  RL arg -> do
    bitShiftCarryOp rotateLeft (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    (,) (getTime True t) <$> prefetch

  RLA -> do
    bitShiftCarryOp rotateLeft (\v' c' -> 0x00 & flagC .~ c') (OutReg8 A)
    (,) (getTime True t) <$> prefetch

  RR arg -> do
    bitShiftCarryOp rotateRight (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    (,) (getTime True t) <$> prefetch

  RRA -> do
    bitShiftCarryOp rotateRight (\v' c' -> 0x00 & flagC .~ c') (OutReg8 A)
    (,) (getTime True t) <$> prefetch

  RLCA -> do
    bitShiftOp rotateLeftCarry (\_ c' -> 0x00 & flagC .~ c') (OutReg8 A)
    (,) (getTime True t) <$> prefetch

  RRCA -> do
    bitShiftOp rotateRightCarry (\_ c' -> 0x00 & flagC .~ c') (OutReg8 A)
    (,) (getTime True t) <$> prefetch

  RLC arg -> do
    bitShiftOp rotateLeftCarry (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    (,) (getTime True t) <$> prefetch

  RRC arg -> do
    bitShiftOp rotateRightCarry (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    (,) (getTime True t) <$> prefetch

  SRL arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v `shiftR` 1
    setOut8 arg v'
    modifyFlags $ \_ -> 0x00
      & flagC .~ (v `testBit` 0)
      & flagZ .~ (v' == 0)
    (,) (getTime True t) <$> prefetch

  SLA arg -> do
    bitShiftOp shiftLeftArithmetic (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    (,) (getTime True t) <$> prefetch

  SRA arg -> do
    bitShiftOp shiftRightArithmetic (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    (,) (getTime True t) <$> prefetch

  JR f -> do
    t' <- getFlag f
    r <- sbyte
    when t' $ jumpRelative r
    (,) (getTime t' t) <$> prefetch

  JP f addr -> do
    t' <- getFlag f
    addr <- getAddress addr
    when t' $ storePC addr
    (,) (getTime t' t) <$> prefetch

  CALL f -> do
    t' <- getFlag f
    when t' . call =<< word
    (,) (getTime t' t) <$> prefetch

  RET f -> do
    t' <- getFlag f
    when t' ret
    (,) (getTime t' t) <$> prefetch

  RETI -> do
    setIME True
    ret
    (,) (getTime True t) <$> prefetch

  RST g -> do
    restart $ (* 8) g
    (,) (getTime True t) <$> prefetch

  PUSH reg -> do
    push =<< loadReg16 reg
    (,) (getTime True t) <$> prefetch
  POP reg -> do
    pop >>= storeReg16 reg
    when (reg == AF) (modifyFlags (.&. 0xF0))
    (,) (getTime True t) <$> prefetch

  ADD arg -> do
    arith add arg False
    (,) (getTime True t) <$> prefetch

  ADD16_HL from -> do
    v <- loadReg16 HL
    dv <- getIn16 from
    let v' = v + dv
    storeReg16 HL v'
    modifyFlags $ \f -> f
      & flagN .~ False
      & flagC .~ (v' < v)
      & flagH .~ ((v' .&. 0x0FFF) < (v .&. 0x0FFF))
    (,) (getTime True t) <$> prefetch

  ADD16_SP -> do
    v <- loadSP
    dv <- sbyte
    let v' = addRelative v dv
    storeSP v'
    modifyFlags $ \f -> 0x00
      & flagC .~ ((v' .&. 0xFF) < (v .&. 0xFF))
      & flagH .~ ((v' .&. 0x0F) < (v .&. 0x0F))
    (,) (getTime True t) <$> prefetch

  SUB arg -> do
    arith sub arg False
    (,) (getTime True t) <$> prefetch

  ADC arg -> do
    arith add arg True
    (,) (getTime True t) <$> prefetch

  SBC arg -> do
    arith sub arg True
    (,) (getTime True t) <$> prefetch

  CP arg -> do
    k <- getIn8 arg
    a <- loadReg A
    let (_, f) = sub a k False
    storeReg F f
    (,) (getTime True t) <$> prefetch

  INC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v + 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0xFF)
      & flagN .~ False
      & flagH .~ (v .&. 0x0F == 0x0F)
    (,) (getTime True t) <$> prefetch

  INC16 arg -> do
    setOut16 arg . (+1) =<< getIn16 (out16ToIn16 arg)
    (,) (getTime True t) <$> prefetch

  DEC16 arg -> do
    setOut16 arg . subtract 1 =<< getIn16 (out16ToIn16 arg)
    (,) (getTime True t) <$> prefetch

  DEC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v - 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0x01)
      & flagN .~ True
      & flagH .~ (v .&. 0x0F == 0x00)
    (,) (getTime True t) <$> prefetch

  DI -> do
    setIME False
    (,) (getTime True t) <$> prefetch

  EI -> do
    setIME True
    (,) (getTime True t) <$> prefetch

  DAA -> do
    daa
    (,) (getTime True t) <$> prefetch

  CPL -> do
    storeReg A . complement =<< loadReg A
    modifyFlags $ \f -> f
      & flagH .~ True
      & flagN .~ True
    (,) (getTime True t) <$> prefetch

  CCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC %~ not
    (,) (getTime True t) <$> prefetch

  SCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC .~ True
    (,) (getTime True t) <$> prefetch

  HALT -> do
    i <- anyInterrupts
    ime <- getIME
    out <- if not ime && has _Just i then loadPC >>= fmap Running . loadAddr else return Halt
    return (getTime True t, out)

  _ -> error $ "failed at " ++ show instr

module Instruction.Interpret
  ( instructions
  , module Control.Comonad.Cofree
  )
where

import Data.Word
import Data.Bits

import Control.Lens hiding (op, to, from, (:<))
import Control.Monad
import Control.Applicative

import Instruction.Instruction
import Instruction.InOut
import Instruction.Ops
import Instruction.Flag

import MonadEmulator.EmulatorT
import MonadEmulator.Operations

import Control.Comonad.Cofree

data InterpretState = Run (Instruction InstructionExpr Flag Word) | Halt | Interrupt' Interrupt

interpretM :: (MonadEmulator m, Show a) => Instruction InstructionExpr Bool a -> m InterpretState
interpretM instr = case instructionExpr instr of
  NOP -> prefetch

  LD from to -> do
    setOut8 to =<< getIn8 from
    prefetch

  LD16 from to -> do
    setOut16 to =<< getIn16 from
    prefetch

  LD16_SP_HL -> do
    sp <- loadSP
    r <- sbyte
    let v = addRelative sp r
    storeReg16 HL v
    storeReg F $ 0x00
      & flagC .~ ((v .&. 0xFF) < (sp .&. 0xFF))
      & flagH .~ ((v .&. 0x0F) < (sp .&. 0x0F))
    prefetch

  AND arg -> do
    logicOp (.&.) (\a' -> 0x20 & flagZ .~ (a' == 0)) arg
    prefetch

  OR arg -> do
    logicOp (.|.) (\a' -> 0x00 & flagZ .~ (a' == 0)) arg
    prefetch

  XOR arg -> do
    logicOp xor (\a' -> 0x00 & flagZ .~ (a' == 0)) arg
    prefetch

  {- 0xCB instructions and specialization for A -}
  BIT y arg -> do
    v <- getIn8 arg
    modifyFlags $ \f -> f
      & flagZ .~ not (v `testBit` fromIntegral y)
      & flagN .~ False
      & flagH .~ True
    prefetch

  SWAP arg -> do
    x <- getIn8 (outToIn arg)
    let x' = ((x `shiftL` 4) .&. 0xF0) .|. ((x `shiftR` 4) .&. 0x0F)
    setOut8 arg x'
    modifyFlags $ \_ -> 0x00 & flagZ .~ (x' == 0)
    prefetch

  RES bidx arg -> do
    setOut8 arg . (`clearBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    prefetch

  SET bidx arg -> do
    setOut8 arg . (`setBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    prefetch

  RL arg -> do
    bitShiftCarryOp rotateLeft (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    prefetch

  RLA -> do
    bitShiftCarryOp rotateLeft (\v' c' -> 0x00 & flagC .~ c') (OutReg8 A)
    prefetch

  RR arg -> do
    bitShiftCarryOp rotateRight (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    prefetch

  RRA -> do
    bitShiftCarryOp rotateRight (\v' c' -> 0x00 & flagC .~ c') (OutReg8 A)
    prefetch

  RLCA -> do
    bitShiftOp rotateLeftCarry (\_ c' -> 0x00 & flagC .~ c') (OutReg8 A)
    prefetch

  RRCA -> do
    bitShiftOp rotateRightCarry (\_ c' -> 0x00 & flagC .~ c') (OutReg8 A)
    prefetch

  RLC arg -> do
    bitShiftOp rotateLeftCarry (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    prefetch

  RRC arg -> do
    bitShiftOp rotateRightCarry (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    prefetch

  SRL arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v `shiftR` 1
    setOut8 arg v'
    modifyFlags $ \_ -> 0x00
      & flagC .~ (v `testBit` 0)
      & flagZ .~ (v' == 0)
    prefetch

  SLA arg -> do
    bitShiftOp shiftLeftArithmetic (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    prefetch

  SRA arg -> do
    bitShiftOp shiftRightArithmetic (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    prefetch

  JR -> do
    let flag' = maybe True id (instructionFlag instr)
    when flag' . jumpRelative =<< sbyte
    prefetch

  JP addr -> do
    let flag' = maybe True id (instructionFlag instr)
    when flag' . storePC =<< getAddress addr
    prefetch

  CALL -> do
    let flag' = maybe True id (instructionFlag instr)
    when flag' . call =<< word
    prefetch

  RET -> do
    let flag' = maybe True id (instructionFlag instr)
    when flag' ret
    prefetch

  RETI -> do
    setIME True
    ret
    prefetch

  RST g -> do
    restart $ (* 8) g
    prefetch

  PUSH reg -> do
    push =<< loadReg16 reg
    prefetch
  POP reg -> do
    pop >>= storeReg16 reg
    when (reg == AF) (modifyFlags (.&. 0xF0))
    prefetch

  ADD arg -> do
    arith add arg False
    prefetch

  ADD16_HL from -> do
    v <- loadReg16 HL
    dv <- getIn16 from
    let v' = v + dv
    storeReg16 HL v'
    modifyFlags $ \f -> f
      & flagN .~ False
      & flagC .~ (v' < v)
      & flagH .~ ((v' .&. 0x0FFF) < (v .&. 0x0FFF))
    prefetch

  ADD16_SP -> do
    v <- loadSP
    dv <- sbyte
    let v' = addRelative v dv
    storeSP v'
    modifyFlags $ \f -> 0x00
      & flagC .~ ((v' .&. 0xFF) < (v .&. 0xFF))
      & flagH .~ ((v' .&. 0x0F) < (v .&. 0x0F))
    prefetch

  SUB arg -> do
    arith sub arg False
    prefetch

  ADC arg -> do
    arith add arg True
    prefetch

  SBC arg -> do
    arith sub arg True
    prefetch

  CP arg -> do
    k <- getIn8 arg
    a <- loadReg A
    let (_, f) = sub a k False
    storeReg F f
    prefetch

  INC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v + 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0xFF)
      & flagN .~ False
      & flagH .~ (v .&. 0x0F == 0x0F)
    prefetch

  INC16 arg -> do
    setOut16 arg . (+1) =<< getIn16 (out16ToIn16 arg)
    prefetch

  DEC16 arg -> do
    setOut16 arg . subtract 1 =<< getIn16 (out16ToIn16 arg)
    prefetch

  DEC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v - 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0x01)
      & flagN .~ True
      & flagH .~ (v .&. 0x0F == 0x00)
    prefetch

  DI -> do
    setIME False
    prefetch

  EI -> do
    setIME True
    prefetch

  DAA -> do
    daa
    prefetch

  CPL -> do
    storeReg A . complement =<< loadReg A
    modifyFlags $ \f -> f
      & flagH .~ True
      & flagN .~ True
    prefetch

  CCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC %~ not
    prefetch

  SCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC .~ True
    prefetch

  HALT -> do
    i <- anyInterrupts
    ime <- getIME
    if not ime && has _Just i then haltBug else return Halt

  _ -> error $ "failed at " ++ show instr

haltBug :: MonadEmulator m => m InterpretState
haltBug = loadPC >>= loadAddr >>= fmap Run . parseInstructionM

prefetch :: MonadEmulator m => m InterpretState
prefetch = do
  i <- anyInterrupts
  ime <- getIME
  maybe (Run <$> fetch) (return . Interrupt') (guard ime *> i)

fetch :: MonadEmulator m => m (Instruction InstructionExpr Flag Word)
fetch = parseInstructionM =<< byte

instructionEvalFlag :: MonadEmulator m => Instruction i Flag a -> m (Instruction i Bool a)
instructionEvalFlag (InstructionNode t e) = return $! InstructionNode t e
instructionEvalFlag (InstructionBranch tdef tbranch f e) = do
  flag' <- getFlag f
  return $! InstructionBranch tdef tbranch flag' e

interpretStateM :: MonadEmulator m => InterpretState -> m (Word, InterpretState)
interpretStateM (Run op) = do
  op' <- instructionEvalFlag op
  (,) (instructionBranch op') <$> interpretM op'
interpretStateM (Interrupt' int) = (,) 20 . Run <$> (serviceInterrupt int *> fetch)
interpretStateM Halt = (,) 4 <$> do
  i <- anyInterrupts
  case i of
    Just int -> do
      ime <- getIME
      if ime then return $! Interrupt' int else Run <$> fetch
    _      -> return $! Halt

{-# SPECIALIZE instructions :: Emulator (Cofree Emulator Word) #-}
instructions :: MonadEmulator m => m (Cofree m Word)
instructions = go . Run =<< fetch where
  go s = do
    (dt ,s') <- interpretStateM s
    return $! dt :< go s'

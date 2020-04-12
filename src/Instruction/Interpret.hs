module Instruction.Interpret
  ( nextInstruction
  , module Control.Comonad.Cofree
  )
where

import Data.Word
import Data.Bits

import Control.Lens hiding (op, to, from, (:<))
import Control.Monad
import Control.Applicative

import Instruction.Instruction
import Instruction.Time
import Instruction.InOut
import Instruction.Ops
import Instruction.Flag

import MonadEmulator.EmulatorT
import MonadEmulator.Operations

import Control.Comonad.Cofree

{-# SPECIALIZE interpretM :: Instruction -> Emulator (Cofree Emulator Word) #-}
interpretM :: (MonadEmulator m) => Instruction -> m (Cofree m Word)
interpretM instr@(Instruction t op) = case op of
  NOP -> return $! getTime True t :< prefetch

  LD from to -> do
    setOut8 to =<< getIn8 from
    return $! getTime True t :< prefetch

  LD16 from to -> do
    setOut16 to =<< getIn16 from
    return $! getTime True t :< prefetch

  LD16_SP_HL -> do
    sp <- loadSP
    r <- sbyte
    let v = addRelative sp r
    storeReg16 HL v
    storeReg F $ 0x00
      & flagC .~ ((v .&. 0xFF) < (sp .&. 0xFF))
      & flagH .~ ((v .&. 0x0F) < (sp .&. 0x0F))
    return $! getTime True t :< prefetch

  AND arg -> do
    logicOp (.&.) (\a' -> 0x20 & flagZ .~ (a' == 0)) arg
    return $! getTime True t :< prefetch

  OR arg -> do
    logicOp (.|.) (\a' -> 0x00 & flagZ .~ (a' == 0)) arg
    return $! getTime True t :< prefetch

  XOR arg -> do
    logicOp xor (\a' -> 0x00 & flagZ .~ (a' == 0)) arg
    return $! getTime True t :< prefetch

  {- 0xCB instructions and specialization for A -}
  BIT y arg -> do
    v <- getIn8 arg
    modifyFlags $ \f -> f
      & flagZ .~ not (v `testBit` fromIntegral y)
      & flagN .~ False
      & flagH .~ True
    return $! getTime True t :< prefetch

  SWAP arg -> do
    x <- getIn8 (outToIn arg)
    let x' = ((x `shiftL` 4) .&. 0xF0) .|. ((x `shiftR` 4) .&. 0x0F)
    setOut8 arg x'
    modifyFlags $ \_ -> 0x00 & flagZ .~ (x' == 0)
    return $! getTime True t :< prefetch

  RES bidx arg -> do
    setOut8 arg . (`clearBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    return $! getTime True t :< prefetch

  SET bidx arg -> do
    setOut8 arg . (`setBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    return $! getTime True t :< prefetch

  RL arg -> do
    bitShiftCarryOp rotateLeft (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    return $! getTime True t :< prefetch

  RLA -> do
    bitShiftCarryOp rotateLeft (\v' c' -> 0x00 & flagC .~ c') (OutReg8 A)
    return $! getTime True t :< prefetch

  RR arg -> do
    bitShiftCarryOp rotateRight (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    return $! getTime True t :< prefetch

  RRA -> do
    bitShiftCarryOp rotateRight (\v' c' -> 0x00 & flagC .~ c') (OutReg8 A)
    return $! getTime True t :< prefetch

  RLCA -> do
    bitShiftOp rotateLeftCarry (\_ c' -> 0x00 & flagC .~ c') (OutReg8 A)
    return $! getTime True t :< prefetch

  RRCA -> do
    bitShiftOp rotateRightCarry (\_ c' -> 0x00 & flagC .~ c') (OutReg8 A)
    return $! getTime True t :< prefetch

  RLC arg -> do
    bitShiftOp rotateLeftCarry (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    return $! getTime True t :< prefetch

  RRC arg -> do
    bitShiftOp rotateRightCarry (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    return $! getTime True t :< prefetch

  SRL arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v `shiftR` 1
    setOut8 arg v'
    modifyFlags $ \_ -> 0x00
      & flagC .~ (v `testBit` 0)
      & flagZ .~ (v' == 0)
    return $! getTime True t :< prefetch

  SLA arg -> do
    bitShiftOp shiftLeftArithmetic (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    return $! getTime True t :< prefetch

  SRA arg -> do
    bitShiftOp shiftRightArithmetic (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0)) arg
    return $! getTime True t :< prefetch

  JR f -> do
    t' <- getFlag f
    r <- sbyte
    when t' $ jumpRelative r
    return $! getTime t' t :< prefetch

  JP f addr -> do
    t' <- getFlag f
    addr <- getAddress addr
    when t' $ storePC addr
    return $! getTime t' t :< prefetch

  CALL f -> do
    t' <- getFlag f
    when t' . call =<< word
    return $! getTime t' t :< prefetch

  RET f -> do
    t' <- getFlag f
    when t' ret
    return $! getTime t' t :< prefetch

  RETI -> do
    setIME True
    ret
    return $! getTime True t :< prefetch

  RST g -> do
    restart $ (* 8) g
    return $! getTime True t :< prefetch

  PUSH reg -> do
    push =<< loadReg16 reg
    return $! getTime True t :< prefetch
  POP reg -> do
    pop >>= storeReg16 reg
    when (reg == AF) (modifyFlags (.&. 0xF0))
    return $! getTime True t :< prefetch

  ADD arg -> do
    arith add arg False
    return $! getTime True t :< prefetch

  ADD16_HL from -> do
    v <- loadReg16 HL
    dv <- getIn16 from
    let v' = v + dv
    storeReg16 HL v'
    modifyFlags $ \f -> f
      & flagN .~ False
      & flagC .~ (v' < v)
      & flagH .~ ((v' .&. 0x0FFF) < (v .&. 0x0FFF))
    return $! getTime True t :< prefetch

  ADD16_SP -> do
    v <- loadSP
    dv <- sbyte
    let v' = addRelative v dv
    storeSP v'
    modifyFlags $ \f -> 0x00
      & flagC .~ ((v' .&. 0xFF) < (v .&. 0xFF))
      & flagH .~ ((v' .&. 0x0F) < (v .&. 0x0F))
    return $! getTime True t :< prefetch

  SUB arg -> do
    arith sub arg False
    return $! getTime True t :< prefetch

  ADC arg -> do
    arith add arg True
    return $! getTime True t :< prefetch

  SBC arg -> do
    arith sub arg True
    return $! getTime True t :< prefetch

  CP arg -> do
    k <- getIn8 arg
    a <- loadReg A
    let (_, f) = sub a k False
    storeReg F f
    return $! getTime True t :< prefetch

  INC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v + 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0xFF)
      & flagN .~ False
      & flagH .~ (v .&. 0x0F == 0x0F)
    return $! getTime True t :< prefetch

  INC16 arg -> do
    setOut16 arg . (+1) =<< getIn16 (out16ToIn16 arg)
    return $! getTime True t :< prefetch

  DEC16 arg -> do
    setOut16 arg . subtract 1 =<< getIn16 (out16ToIn16 arg)
    return $! getTime True t :< prefetch

  DEC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v - 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0x01)
      & flagN .~ True
      & flagH .~ (v .&. 0x0F == 0x00)
    return $! getTime True t :< prefetch

  DI -> do
    setIME False
    return $! getTime True t :< prefetch

  EI -> do
    setIME True
    return $! getTime True t :< prefetch

  DAA -> do
    daa
    return $! getTime True t :< prefetch

  CPL -> do
    storeReg A . complement =<< loadReg A
    modifyFlags $ \f -> f
      & flagH .~ True
      & flagN .~ True
    return $! getTime True t :< prefetch

  CCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC %~ not
    return $! getTime True t :< prefetch

  SCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC .~ True
    return $! getTime True t :< prefetch

  HALT -> do
    i <- anyInterrupts
    ime <- getIME
    return $! getTime True t :< if not ime && has _Just i then loadPC >>= loadAddr >>= execute else halt

  _ -> error $ "failed at " ++ show instr

{-# SPECIALIZE execute :: Word8 -> Emulator (Cofree Emulator Word) #-}
execute :: (MonadEmulator m) => Word8 -> m (Cofree m Word)
execute = interpretM <=< parseInstructionM

interrupt :: (MonadEmulator m) => Interrupt -> m (Cofree m Word)
interrupt i = do
  serviceInterrupt i
  return $! 20 :< nextInstruction

halt :: (MonadEmulator m) => m (Cofree m Word)
halt = do
  i <- anyInterrupts
  ime <- getIME
  return $! 4 :< (maybe halt id
                  $   (interrupt <$> (guard ime *> i))
                  <|> (nextInstruction <$ i))

{-# INLINE prefetch #-}
prefetch :: (MonadEmulator m) => m (Cofree m Word)
prefetch = do
  i <- anyInterrupts
  ime <- getIME
  maybe nextInstruction interrupt (guard ime *> i)

{-# SPECIALIZE nextInstruction :: Emulator (Cofree Emulator Word) #-}
nextInstruction :: MonadEmulator m => m (Cofree m Word)
nextInstruction = execute =<< byte

{-# LANGUAGE DeriveTraversable, TemplateHaskell #-}
module Instruction.Interpreter
  ( instructions
  , instructionsTrace
  , module Control.Comonad.Cofree
  ) where

import           Data.Bits
import           Data.Maybe                     ( fromMaybe )
import           Data.Word

import           Control.Lens            hiding ( (:<)
                                                , from
                                                , op
                                                , to
                                                )
import           Control.Monad

import           Instruction.Instruction
import           Instruction.Types.Address
import           Instruction.Types.Flag
import           Instruction.Types.Readable
import           Instruction.Types.Writable

import           Instruction.Interpreter.Ops
import           Instruction.Parser

import           MonadEmulator.EmulatorT
import           MonadEmulator.Operations

import           Control.Comonad.Cofree

data InterpretState a = Run a | Halt | Interrupt' Interrupt
  deriving (Functor, Foldable, Traversable)

makePrisms ''InterpretState

interpretM
  :: (MonadEmulator m, Show a)
  => Instruction Bool a
  -> m (InterpretState (m Instruction'))
interpretM instr = case instr ^. expr of
  NOP        -> prefetch

  LD from to -> do
    write8 to =<< read8 from
    prefetch

  LD16 from to -> do
    write16 to =<< read16 from
    prefetch

  LD16_SP_HL r -> do
    sp <- loadSP
    let v = addRelative sp r
    storeReg16 HL v
    storeReg F
      $  0x00
      &  flagC
      .~ ((v .&. 0xFF) < (sp .&. 0xFF))
      &  flagH
      .~ ((v .&. 0x0F) < (sp .&. 0x0F))
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
    v <- read8 arg
    modifyFlags $ \f ->
      f
        &  flagZ
        .~ not (v `testBit` fromIntegral y)
        &  flagN
        .~ False
        &  flagH
        .~ True
    prefetch

  SWAP arg -> do
    x <- read8 (readable8 arg)
    let x' = ((x `shiftL` 4) .&. 0xF0) .|. ((x `shiftR` 4) .&. 0x0F)
    write8 arg x'
    modifyFlags $ \_ -> 0x00 & flagZ .~ (x' == 0)
    prefetch

  RES bidx arg -> do
    write8 arg . (`clearBit` fromIntegral bidx) =<< read8 (readable8 arg)
    prefetch

  SET bidx arg -> do
    write8 arg . (`setBit` fromIntegral bidx) =<< read8 (readable8 arg)
    prefetch

  RL arg -> do
    bitShiftCarryOp rotateLeft
                    (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0))
                    arg
    prefetch

  RLA -> do
    bitShiftCarryOp rotateLeft (\_ c' -> 0x00 & flagC .~ c') (WriteReg8 A)
    prefetch

  RR arg -> do
    bitShiftCarryOp rotateRight
                    (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0))
                    arg
    prefetch

  RRA -> do
    bitShiftCarryOp rotateRight (\_ c' -> 0x00 & flagC .~ c') (WriteReg8 A)
    prefetch

  RLCA -> do
    bitShiftOp rotateLeftCarry (\_ c' -> 0x00 & flagC .~ c') (WriteReg8 A)
    prefetch

  RRCA -> do
    bitShiftOp rotateRightCarry (\_ c' -> 0x00 & flagC .~ c') (WriteReg8 A)
    prefetch

  RLC arg -> do
    bitShiftOp rotateLeftCarry
               (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0))
               arg
    prefetch

  RRC arg -> do
    bitShiftOp rotateRightCarry
               (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0))
               arg
    prefetch

  SRL arg -> do
    v <- read8 (readable8 arg)
    let v' = v `shiftR` 1
    write8 arg v'
    modifyFlags $ \_ -> 0x00 & flagC .~ (v `testBit` 0) & flagZ .~ (v' == 0)
    prefetch

  SLA arg -> do
    bitShiftOp shiftLeftArithmetic
               (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0))
               arg
    prefetch

  SRA arg -> do
    bitShiftOp shiftRightArithmetic
               (\v' c' -> 0x00 & flagC .~ c' & flagZ .~ (v' == 0))
               arg
    prefetch

  JR offset -> do
    let flag' = andOf flag instr
    when flag' $ jumpRelative offset
    prefetch

  JP addr -> do
    let flag' = andOf flag instr
    when flag' . storePC =<< getAddress addr
    prefetch

  CALL addr -> do
    let flag' = andOf flag instr
    when flag' $ call addr
    prefetch

  RET -> do
    let flag' = andOf flag instr
    when flag' ret
    prefetch

  RETI -> do
    setIME True
    ret
    prefetch

  RST g -> do
    restart g
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
    v  <- loadReg16 HL
    dv <- read16 from
    let v' = v + dv
    storeReg16 HL v'
    modifyFlags $ \f ->
      f
        &  flagN
        .~ False
        &  flagC
        .~ (v' < v)
        &  flagH
        .~ ((v' .&. 0x0FFF) < (v .&. 0x0FFF))
    prefetch

  ADD16_SP -> do
    v  <- loadSP
    dv <- sbyte
    let v' = addRelative v dv
    storeSP v'
    storeReg F
      $  0x00
      &  flagC
      .~ ((v' .&. 0xFF) < (v .&. 0xFF))
      &  flagH
      .~ ((v' .&. 0x0F) < (v .&. 0x0F))
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
    k <- read8 arg
    a <- loadReg A
    let (_, f) = sub a k False
    storeReg F f
    prefetch

  INC arg -> do
    v <- read8 (readable8 arg)
    let v' = v + 1
    write8 arg v'
    modifyFlags
      $ \f ->
          f
            &  flagZ
            .~ (v == 0xFF)
            &  flagN
            .~ False
            &  flagH
            .~ (v .&. 0x0F == 0x0F)
    prefetch

  INC16 arg -> do
    write16 arg . (+ 1) =<< read16 (readable16 arg)
    prefetch

  DEC16 arg -> do
    write16 arg . subtract 1 =<< read16 (readable16 arg)
    prefetch

  DEC arg -> do
    v <- read8 (readable8 arg)
    let v' = v - 1
    write8 arg v'
    modifyFlags $ \f ->
      f & flagZ .~ (v == 0x01) & flagN .~ True & flagH .~ (v .&. 0x0F == 0x00)
    prefetch

  DI  -> setIME False *> prefetch
  EI  -> prefetch <* setIME True

  DAA -> daa *> prefetch

  CPL -> do
    storeReg A . complement =<< loadReg A
    modifyFlags $ \f -> f & flagH .~ True & flagN .~ True
    prefetch

  CCF -> do
    modifyFlags $ \f -> f & flagH .~ False & flagN .~ False & flagC %~ not
    prefetch

  SCF -> do
    modifyFlags $ \f -> f & flagH .~ False & flagN .~ False & flagC .~ True
    prefetch

  HALT -> do
    i   <- anyInterrupts
    ime <- getIME
    if not ime && has _Just i then haltBug else return Halt

  STOP -> error "STOP"

haltBug :: MonadEmulator m => m (InterpretState (m Instruction'))
haltBug = do
  b <- loadPC >>= loadAddr
  return $! Run $ parseInstructionM b

prefetch :: MonadEmulator m => m (InterpretState (m Instruction'))
prefetch = do
  i   <- anyInterrupts
  ime <- getIME
  maybe (Run <$> fetch) (return . Interrupt') (guard ime *> i)

fetch :: MonadEmulator m => m (m Instruction')
fetch = parseInstructionM <$> byte

{-# INLINE interpretStateM #-}
interpretStateM
  :: MonadEmulator m
  => InterpretState Instruction'
  -> m (Word, InterpretState (m Instruction'))
interpretStateM (Run op) = do
  op' <- traverseOf flag evalFlag op
  (,) (op' ^. branch) <$> interpretM op'
interpretStateM (Interrupt' int) =
  (,) 20 . Run <$> (serviceInterrupt int *> fetch)
interpretStateM Halt = (,) 4 <$> do
  i <- anyInterrupts
  fmap (fromMaybe Halt) $ forM i $ \int -> do
    ime <- getIME
    if ime then return $! Interrupt' int else Run <$> fetch

{-# SPECIALIZE instructions :: Emulator (Cofree Emulator Word) #-}
instructions :: MonadEmulator m => m (Cofree m Word)
instructions = go . Run =<< fetch where
  go s = do
    (dt, s') <- interpretStateM =<< sequenceA s
    return $! dt :< go s'

{-# SPECIALIZE instructionsTrace :: Emulator (Cofree Emulator (Word, Maybe (Word16, Instruction'))) #-}
instructionsTrace
  :: MonadEmulator m => m (Cofree m (Word, Maybe (Word16, Instruction')))
instructionsTrace = go . Run =<< fetch where
  go s = do
    pc    <- subtract 1 <$> loadPC
    sEval <- sequenceA s
    let instr = (pc, sEval) ^? aside _Run
    (dt, s') <- interpretStateM sEval
    return $! (dt, instr) :< go s'

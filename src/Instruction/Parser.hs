module Instruction.Parser
  ( parseInstructionM
  , readable8
  , readable16
  ) where

import           Data.Bits
import           Data.Word

import           Text.Printf

import           MonadEmulator.EmulatorT
import           MonadEmulator.Operations

import           Instruction.Types.Address
import           Instruction.Types.Flag
import           Instruction.Types.Readable
import           Instruction.Types.Writable

import           Instruction.Instruction

-- | decompose byte to xxyyyzzz
byteCodeDecompose :: Word8 -> (Word8, Word8, Word8)
byteCodeDecompose b =
  ((b `shiftR` 6) .&. 0x3, (b `shiftR` 3) .&. 0x7, b .&. 0x7)

readable8 :: Writable8 -> Readable8
readable8 (WriteReg8  r   ) = ReadReg8 r
readable8 (WriteAddr8 addr) = ReadAddr8 addr

readable16 :: Writable16 -> Readable16
readable16 (WriteReg16 r)        = ReadReg16 r
readable16 WriteSP               = ReadSP
readable16 (WriteImmAddr16 addr) = ReadImmAddr16 addr

basicRegisterArg :: Word8 -> Writable8
basicRegisterArg w = case w of
  0 -> WriteReg8 B
  1 -> WriteReg8 C
  2 -> WriteReg8 D
  3 -> WriteReg8 E
  4 -> WriteReg8 H
  5 -> WriteReg8 L
  6 -> WriteAddr8 AddrHL
  7 -> WriteReg8 A
  _ -> error "basicRegisterArg: this should not be possible"

registerPointerArg :: Word8 -> Addr
registerPointerArg y = case y .&. 0x6 of
  0 -> AddrBC
  2 -> AddrDE
  4 -> AddrHLi
  6 -> AddrHLd
  _ -> error "registerPointer: invalid argument"

argWritable16 :: Word8 -> Writable16
argWritable16 w = case w .&. 0x6 of
  0 -> WriteReg16 BC
  2 -> WriteReg16 DE
  4 -> WriteReg16 HL
  6 -> WriteSP
  _ -> error "impossible"

{-# INLINE aluMnemonic #-}
aluMnemonic :: Word8 -> Readable8 -> Expr
aluMnemonic w arg = case w of
  0 -> ADD arg
  1 -> ADC arg
  2 -> SUB arg
  3 -> SBC arg
  4 -> AND arg
  5 -> XOR arg
  6 -> OR arg
  7 -> CP arg
  _ -> error "aluMnemonic: invalid argument"

parseExtendedInstruction :: Word8 -> Instruction f Word
parseExtendedInstruction b =
  let {-# INLINE o #-}
      o = InstructionNode
  in  case byteCodeDecompose b of
        (0, y, z) ->
          let op = case y of
                0 -> RLC
                1 -> RRC
                2 -> RL
                3 -> RR
                4 -> SLA
                5 -> SRA
                6 -> SWAP
                _ -> SRL
          in  o (if z == 6 then 16 else 8) (op (basicRegisterArg z))

        (1, y, z) ->
          o (if z == 6 then 12 else 8) (BIT y $ readable8 $ basicRegisterArg z)
        (2, y, z) -> o (if z == 6 then 16 else 8) (RES y $ basicRegisterArg z)
        (3, y, z) -> o (if z == 6 then 16 else 8) (SET y $ basicRegisterArg z)
        _         -> error $ printf "unknown bytecode 0x%02x" b

{-# SPECIALIZE parseInstructionM :: Word8 -> Emulator Instruction' #-}
parseInstructionM :: MonadEmulator m => Word8 -> m Instruction'
parseInstructionM b =

  let
    {-# INLINE o #-}
    o = InstructionNode
  in
    case byteCodeDecompose b of

      (0, 0, 0) -> return $! o 4 NOP
      (0, 2, 0) -> return $! o 4 STOP
      (0, 1, 0) -> do
        addr <- word
        return $! o 20 (LD16 ReadSP (WriteImmAddr16 addr))
      (0, 3, 0) -> do
        dat <- sbyte
        return $! o 12 (JR dat)
      (0, y, 0) -> do
        dat <- sbyte
        return $! InstructionBranch 8 12 (parseFlag (y .&. 0x3)) (JR' dat)

      (0, y, 1)
        | y `testBit` 0 -> return $! o 8 $ ADD16_HL $ readable16
          (argWritable16 y)
        | otherwise -> do
          dat <- word
          return $! o 12 $ LD16 (ReadImm16 dat) (argWritable16 y)

      (0, y, 2)
        | y `testBit` 0 -> return $! o 8 $ LD
          (ReadAddr8 $ registerPointerArg y)
          (WriteReg8 A)
        | otherwise -> return $! o 8 $ LD (ReadReg8 A)
                                          (WriteAddr8 $ registerPointerArg y)

      (0, y, 3) | y `testBit` 0 -> return $! o 8 $ DEC16 $ argWritable16 y
                | otherwise     -> return $! o 8 $ INC16 $ argWritable16 y

      (0, y, 4) ->
        return $! o (if y == 6 then 12 else 4) $ INC (basicRegisterArg y)
      (0, y, 5) ->
        return $! o (if y == 6 then 12 else 4) $ DEC (basicRegisterArg y)
      (0, y, 6) -> do
        dat <- byte
        return $! o (if y == 6 then 12 else 8) $ LD (ReadImm8 dat)
                                                    (basicRegisterArg y)

      (0, 0, 7) -> return $! o 4 RLCA
      (0, 1, 7) -> return $! o 4 RRCA
      (0, 2, 7) -> return $! o 4 RLA
      (0, 3, 7) -> return $! o 4 RRA

      (0, 4, 7) -> return $! o 4 DAA
      (0, 5, 7) -> return $! o 4 CPL
      (0, 6, 7) -> return $! o 4 SCF
      (0, 7, 7) -> return $! o 4 CCF

      (1, 6, 6) -> return $! o 4 HALT
      (1, y, z) -> return $! o (if y == 6 || z == 6 then 8 else 4) $ LD
        (readable8 $ basicRegisterArg z)
        (basicRegisterArg y)

      (2, y, z) -> return $! o
        (if z == 6 then 8 else 4)
        (aluMnemonic y $ readable8 $ basicRegisterArg z)

      (3, 4, 0) -> do
        addr <- byte
        return $! o 12 $ LD (ReadReg8 A) (WriteAddr8 (ZeroPage addr))
      (3, 6, 0) -> do
        addr <- byte
        return $! o 12 $ LD (ReadAddr8 (ZeroPage addr)) (WriteReg8 A)
      (3, 5, 0) -> return $! o 16 ADD16_SP
      (3, 7, 0) -> do
        dat <- sbyte
        return $! o 12 (LD16_SP_HL dat)
      (3, y, 0) -> return $! InstructionBranch 8 20 (parseFlag y) RET'

      (3, 0, 1) -> return $! o 12 $ POP BC
      (3, 2, 1) -> return $! o 12 $ POP DE
      (3, 4, 1) -> return $! o 12 $ POP HL
      (3, 6, 1) -> return $! o 12 $ POP AF

      (3, 1, 1) -> return $! o 16 RET
      (3, 3, 1) -> return $! o 16 RETI
      (3, 5, 1) -> return $! o 4 $ JP AddrHL
      (3, 7, 1) -> return $! o 8 $ LD16 (ReadReg16 HL) WriteSP

      (3, 4, 2) -> return $! o 8 $ LD (ReadReg8 A) (WriteAddr8 ZeroPageC)
      (3, 6, 2) -> return $! o 8 $ LD (ReadAddr8 ZeroPageC) (WriteReg8 A)
      (3, 5, 2) -> do
        addr <- AddrDirect <$> word
        return $! o 16 $ LD (ReadReg8 A) (WriteAddr8 addr)
      (3, 7, 2) -> do
        addr <- AddrDirect <$> word
        return $! o 16 $ LD (ReadAddr8 addr) (WriteReg8 A)
      (3, y, 2) -> do
        addr <- AddrDirect <$> word
        return $! InstructionBranch 12 16 (parseFlag y) $ JP' addr

      (3, 0, 3) -> do
        addr <- AddrDirect <$> word
        return $! o 16 (JP addr)
      (3, 1, 3)         -> parseExtendedInstruction <$> byte

      (3, 6, 3)         -> return $! o 4 DI
      (3, 7, 3)         -> return $! o 4 EI

      (3, f, 4) | f < 4 -> do
        addr <- word
        return $! InstructionBranch 12 24 (parseFlag f) $ CALL' addr

      (3, 0, 5) -> return $! o 16 $ PUSH BC
      (3, 2, 5) -> return $! o 16 $ PUSH DE
      (3, 4, 5) -> return $! o 16 $ PUSH HL
      (3, 6, 5) -> return $! o 16 $ PUSH AF
      (3, 1, 5) -> do
        addr <- word
        return $! o 24 $ CALL addr

      (3, y, 6) -> do
        dat <- byte
        return $! o 8 $ aluMnemonic y (ReadImm8 dat)
      (3, y, 7) -> return $! o 16 $ RST (8 * y)
      _         -> error $ printf "unknown bytecode 0x%02x" b

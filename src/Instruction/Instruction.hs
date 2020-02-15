module Instruction.Instruction where

import Instruction.Time
import Instruction.InOut
import Instruction.Flag

import Text.Printf
import Data.Word
import Data.Bits

import MonadEmulator

-- | decompose byte to xxyyyzzz
byteCodeDecompose :: Word8 -> (Word8, Word8, Word8)
byteCodeDecompose b =
  ((b `shiftR` 6) .&. 0x3, (b `shiftR` 3) .&. 0x7, b .&. 0x7)
{-# INLINE byteCodeDecompose #-}

data InstructionExpr
  = LD In8 Out8 -- from -> to
  | PUSH Reg16 | POP Reg16

  | JP (Maybe Flag) Addr | JR (Maybe Flag)
  | CALL (Maybe Flag) | RET (Maybe Flag)

  | NOP | STOP | HALT

  | LD16 In16 Out16 -- from -> to
  | LD16_SP_HL -- sp + imm8 = hl
  | INC16 Out16 | DEC16 Out16
  | ADD16_HL In16
  | ADD16_SP

  | INC Out8 | DEC Out8
  | ADD In8 | SUB In8 | ADC In8 | SBC In8
  | CP In8

  | DI | EI | RETI | RST Word8

  | AND In8 | OR In8 | XOR In8
  | RLCA | RRCA | RLA | RRA

  | RLC Out8 | RRC Out8 | RL Out8 | RR Out8
  | SLA Out8 | SRA Out8 | SRL Out8 | SWAP Out8

  | BIT Word8 In8 | SET Word8 Out8 | RES Word8 Out8

  | DAA | CPL | SCF | CCF
  deriving Show

data Instruction
  = Instruction Word8 (Time Word) InstructionExpr
  deriving Show

basicRegisterArg :: Word8 -> Out8
basicRegisterArg w = case w of
  0 -> OutReg8 $ B
  1 -> OutReg8 $ C
  2 -> OutReg8 $ D
  3 -> OutReg8 $ E
  4 -> OutReg8 $ H
  5 -> OutReg8 $ L
  6 -> OutAddr8 $ AddrHL
  7 -> OutReg8 $ A
  _ -> error "basicRegisterArg: this should not be possible"

registerPointerArg :: Word8 -> Addr
registerPointerArg y = case y .&. 0x6 of
  0 -> AddrBC
  2 -> AddrDE
  4 -> AddrHLi
  6 -> AddrHLd
  _ -> error "registerPointer: invalid argument"

{-# INLINE aluMnemonic #-}
aluMnemonic :: Word8 -> In8 -> InstructionExpr
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

parseInstructionM :: MonadEmulator m => Word8 -> m Instruction
parseInstructionM 0xCB = parseExtendedInstruction <$> byte
parseInstructionM b = return $ parseInstruction b

parseExtendedInstruction :: Word8 -> Instruction
parseExtendedInstruction b =
  let {-# INLINE o #-}
      o = Instruction b
  in
    case byteCodeDecompose b of
      (0,y,z) ->
        let op = case y of { 0 -> RLC ; 1 -> RRC ; 2 -> RL; 3 -> RR; 4 -> SLA; 5 -> SRA; 6 -> SWAP; _ -> SRL }
        in o (ConstantTime $ if z == 6 then 16 else 8) (op (basicRegisterArg z))

      (1,y,z) -> o (ConstantTime $ if z == 6 then 12 else 8) (BIT y $ outToIn $ basicRegisterArg z)
      (2,y,z) -> o (ConstantTime $ if z == 6 then 16 else 8) (RES y $ basicRegisterArg z)
      (3,y,z) -> o (ConstantTime $ if z == 6 then 16 else 8) (SET y $ basicRegisterArg z)
      _ -> error $ printf "unknown bytecode 0x%02x" b

parseInstruction :: Word8 -> Instruction
parseInstruction b =

  let {-# INLINE o #-}
      o = Instruction b
  in
    case byteCodeDecompose b of

    (0,0,0) -> o (ConstantTime 4) NOP
    (0,2,0) -> o (ConstantTime 4) STOP
    (0,1,0) -> o (ConstantTime 20) (LD16 InSP OutImmAddr16)
    (0,3,0) -> o (ConstantTime 12) (JR Nothing)
    (0,y,0) -> o (VariableTime 8 12) (JR (Just $ flag (y .&. 0x3)))

    (0,0,1) -> o (ConstantTime 12) $ LD16 InImm16 (OutReg16 BC)
    (0,2,1) -> o (ConstantTime 12) $ LD16 InImm16 (OutReg16 DE)
    (0,4,1) -> o (ConstantTime 12) $ LD16 InImm16 (OutReg16 HL)
    (0,6,1) -> o (ConstantTime 12) $ LD16 InImm16 OutSP

    (0,1,1) -> o (ConstantTime 8) $ ADD16_HL $ InReg16 BC
    (0,3,1) -> o (ConstantTime 8) $ ADD16_HL $ InReg16 DE
    (0,5,1) -> o (ConstantTime 8) $ ADD16_HL $ InReg16 HL
    (0,7,1) -> o (ConstantTime 8) $ ADD16_HL $ InSP

    (0,y@1,2) -> o (ConstantTime 8) $ LD (InAddr8 $ registerPointerArg y) (OutReg8 A)
    (0,y@3,2) -> o (ConstantTime 8) $ LD (InAddr8 $ registerPointerArg y) (OutReg8 A)
    (0,y@5,2) -> o (ConstantTime 8) $ LD (InAddr8 $ registerPointerArg y) (OutReg8 A)
    (0,y@7,2) -> o (ConstantTime 8) $ LD (InAddr8 $ registerPointerArg y) (OutReg8 A)

    (0,y@0,2) -> o (ConstantTime 8) $ LD (InReg8 A) (OutAddr8 $ registerPointerArg y)
    (0,y@2,2) -> o (ConstantTime 8) $ LD (InReg8 A) (OutAddr8 $ registerPointerArg y)
    (0,y@4,2) -> o (ConstantTime 8) $ LD (InReg8 A) (OutAddr8 $ registerPointerArg y)
    (0,y@6,2) -> o (ConstantTime 8) $ LD (InReg8 A) (OutAddr8 $ registerPointerArg y)

    (0,0,3) -> o (ConstantTime 8) $ INC16 $ OutReg16 BC
    (0,2,3) -> o (ConstantTime 8) $ INC16 $ OutReg16 DE
    (0,4,3) -> o (ConstantTime 8) $ INC16 $ OutReg16 HL
    (0,6,3) -> o (ConstantTime 8) $ INC16 $ OutSP

    (0,1,3) -> o (ConstantTime 8) $ DEC16 $ OutReg16 BC
    (0,3,3) -> o (ConstantTime 8) $ DEC16 $ OutReg16 DE
    (0,5,3) -> o (ConstantTime 8) $ DEC16 $ OutReg16 HL
    (0,7,3) -> o (ConstantTime 8) $ DEC16 $ OutSP

    (0,y,4) -> o (ConstantTime $ if y == 6 then 12 else 4) $ INC (basicRegisterArg y)
    (0,y,5) -> o (ConstantTime $ if y == 6 then 12 else 4) $ DEC (basicRegisterArg y)
    (0,y,6) -> o (ConstantTime $ if y == 6 then 12 else 8) $ LD InImm8 (basicRegisterArg y)

    (0,0,7) -> o (ConstantTime 4) RLCA
    (0,1,7) -> o (ConstantTime 4) RRCA
    (0,2,7) -> o (ConstantTime 4) RLA
    (0,3,7) -> o (ConstantTime 4) RRA

    (0,4,7) -> o (ConstantTime 4) DAA
    (0,5,7) -> o (ConstantTime 4) CPL
    (0,6,7) -> o (ConstantTime 4) SCF
    (0,7,7) -> o (ConstantTime 4) CCF

    (1,6,6) -> o (ConstantTime 4) HALT
    (1,y,z) -> o (ConstantTime $ if y == 6 || z == 6 then 8 else 4)
      $ LD (outToIn $ basicRegisterArg z) (basicRegisterArg y)

    (2,y,z) -> o (ConstantTime $ if z == 6 then 8 else 4) (aluMnemonic y $ outToIn $ basicRegisterArg z)

    (3,4,0) -> o (ConstantTime 12) $ LD (InReg8 A) (OutAddr8 ZeroPage)
    (3,6,0) -> o (ConstantTime 12) $ LD (InAddr8 ZeroPage) (OutReg8 A)
    (3,5,0) -> o (ConstantTime 16) ADD16_SP
    (3,7,0) -> o (ConstantTime 12) LD16_SP_HL
    (3,y,0) -> o (VariableTime 8 20) $ RET (Just $ flag y)

    (3,0,1) -> o (ConstantTime 12) $ POP BC
    (3,2,1) -> o (ConstantTime 12) $ POP DE
    (3,4,1) -> o (ConstantTime 12) $ POP HL
    (3,6,1) -> o (ConstantTime 12) $ POP AF

    (3,1,1) -> o (ConstantTime 16) $ RET Nothing
    (3,3,1) -> o (ConstantTime 16) RETI
    (3,5,1) -> o (ConstantTime  4) $ JP Nothing AddrHL
    (3,7,1) -> o (ConstantTime  8) $ LD16 (InReg16 HL) OutSP

    (3,4,2) -> o (ConstantTime 8)  $ LD (InReg8 A) (OutAddr8 ZeroPageC)
    (3,6,2) -> o (ConstantTime 8)  $ LD (InAddr8 ZeroPageC) (OutReg8 A)
    (3,5,2) -> o (ConstantTime 16) $ LD (InReg8 A) (OutAddr8 AddrDirect)
    (3,7,2) -> o (ConstantTime 16) $ LD (InAddr8 AddrDirect) (OutReg8 A)
    (3,y,2) -> o (VariableTime 12 16) $ JP (Just $ flag y) AddrDirect

    (3,0,3) -> o (ConstantTime 16) (JP Nothing AddrDirect)
    (3,1,3) -> error "0xCB"

    (3,6,3) -> o (ConstantTime 4) DI
    (3,7,3) -> o (ConstantTime 4) EI

    (3,f@0,4) -> o (VariableTime 12 24) $ CALL (Just $ flag f)
    (3,f@1,4) -> o (VariableTime 12 24) $ CALL (Just $ flag f)
    (3,f@2,4) -> o (VariableTime 12 24) $ CALL (Just $ flag f)
    (3,f@3,4) -> o (VariableTime 12 24) $ CALL (Just $ flag f)

    (3,0,5) -> o (ConstantTime 16) $ PUSH BC
    (3,2,5) -> o (ConstantTime 16) $ PUSH DE
    (3,4,5) -> o (ConstantTime 16) $ PUSH HL
    (3,6,5) -> o (ConstantTime 16) $ PUSH AF
    (3,1,5) -> o (ConstantTime 24) $ CALL Nothing

    (3,y,6) -> o (ConstantTime 8)  $ aluMnemonic y InImm8
    (3,y,7) -> o (ConstantTime 16) $ RST y
    _ -> error $ printf "unknown bytecode 0x%02x" b

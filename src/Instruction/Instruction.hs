module Instruction.Instruction where

import CPU.Registers
import Instruction.Time

import Data.Traversable

import Text.Printf
import Data.Word
import Data.Bits

-- | decompose byte to xxyyyzzz
byteCodeDecompose :: Word8 -> (Word8, Word8, Word8)
byteCodeDecompose b =
  ((b `shiftR` 6) .&. 0x3, (b `shiftR` 3) .&. 0x7, b .&. 0x7)
{-# INLINE byteCodeDecompose #-}

data Arg = ArgDirect8 Reg8
         | ArgDirect16 Reg16
         | ArgSP

         | Immediate8
         | Immediate16

         | Address
         | AddressRel

         | ArgPointerRegFF Reg8
         | ArgPointerReg Reg16
         | ArgPointerImmFF

         | ArgPointerImm8
         | ArgPointerImm16

         | ArgPointerHLi
         | ArgPointerHLd
         deriving (Eq)

instance Show Arg where
  show arg = case arg of
    ArgDirect8 r -> show r
    ArgDirect16 r -> show r
    ArgSP -> "SP"

    Immediate8 -> "d8"
    Immediate16 -> "d16"

    Address    -> "a16"
    AddressRel -> "r8"

    ArgPointerImmFF   -> "($FF00+a8)"
    ArgPointerImm8    -> "(a16)"
    ArgPointerImm16   -> "(a16)"
    ArgPointerRegFF r -> printf "($FF00+%s)" (show r)
    ArgPointerReg r   -> printf "(%s)" (show r)
    ArgPointerHLi     -> "(HL+)"
    ArgPointerHLd     -> "(HL-)"

data Flag = FlagZ | FlagC | FlagNZ | FlagNC
  deriving (Eq)

instance Show Flag where
  show FlagZ  = "Z"
  show FlagNZ = "NZ"
  show FlagC  = "C"
  show FlagNC = "NC"

data InstructionExpr
  = LD Arg Arg -- from -> to
  | PUSH Arg | POP Arg

  | JP (Maybe Flag) Arg | JR (Maybe Flag) Arg
  | CALL (Maybe Flag) Arg | RET (Maybe Flag)

  | NOP | STOP | HALT

  | LD16 Arg Arg -- from -> to
  | LD16_SP_HL -- sp + imm8 = hl
  | INC16 Arg | DEC16 Arg
  | ADD16_HL Arg
  | ADD16_SP

  | INC Arg | DEC Arg
  | ADD Arg | SUB Arg | ADC Arg | SBC Arg
  | CP Arg

  | DI | EI | RETI | RST Word8

  | AND Arg | OR Arg | XOR Arg
  | RLCA | RRCA | RLA | RRA

  | RLC Arg | RRC Arg | RL Arg | RR Arg
  | SLA Arg | SRA Arg | SRL Arg | SWAP Arg

  | BIT Word8 Arg | SET Word8 Arg | RES Word8 Arg

  | DAA | CPL | SCF | CCF
  deriving Show

data Instruction
  = Instruction Word8 (Time Word) InstructionExpr
  deriving Show

{-# INLINE basicRegisterArg #-}
basicRegisterArg :: Word8 -> Arg
basicRegisterArg w = case w of
  0 -> ArgDirect8 B
  1 -> ArgDirect8 C
  2 -> ArgDirect8 D
  3 -> ArgDirect8 E
  4 -> ArgDirect8 H
  5 -> ArgDirect8 L
  6 -> ArgPointerReg HL
  7 -> ArgDirect8 A
  _ -> error "basicRegisterArg: this should not be possible"

{-# INLINE registerPointerArg #-}
registerPointerArg :: Word8 -> Arg
registerPointerArg y = case y .&. 0x6 of
  0 -> ArgPointerReg BC
  2 -> ArgPointerReg DE
  4 -> ArgPointerHLi
  6 -> ArgPointerHLd
  _ -> error "registerPointer: invalid argument"

{-# INLINE aluMnemonic #-}
aluMnemonic :: Word8 -> Arg -> InstructionExpr
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

{-# INLINE flag #-}
flag :: Word8 -> Flag
flag w = case w of
  0 -> FlagNZ
  1 -> FlagZ
  2 -> FlagNC
  3 -> FlagC
  _ -> error "flag: invalid argument"


parseInstructionM :: Monad m => m Word8 -> m Instruction
parseInstructionM get = do
  b <- get
  if b == 0xCB
    then parseExtendedInstruction <$> get
    else return $ parseInstruction b

parseExtendedInstruction :: Word8 -> Instruction
parseExtendedInstruction b =
  let {-# INLINE o #-}
      o = Instruction b
  in
    case byteCodeDecompose b of
      (0,y,z) ->
        let op = case y of { 0 -> RLC ; 1 -> RRC ; 2 -> RL; 3 -> RR; 4 -> SLA; 5 -> SRA; 6 -> SWAP; _ -> SRL }
        in o (ConstantTime $ if z == 6 then 16 else 8) (op (basicRegisterArg z))

      (1,y,z) -> o (ConstantTime $ if z == 6 then 12 else 8) (BIT y $ basicRegisterArg z)
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
    (0,1,0) -> o (ConstantTime 20) (LD16 ArgSP ArgPointerImm16)
    (0,3,0) -> o (ConstantTime 12) (JR Nothing AddressRel)
    (0,y,0) -> o (VariableTime 8 12) (JR (Just $ flag (y .&. 0x3)) AddressRel)

    (0,0,1) -> o (ConstantTime 12) $ LD16 Immediate16 (ArgDirect16 BC)
    (0,2,1) -> o (ConstantTime 12) $ LD16 Immediate16 (ArgDirect16 DE)
    (0,4,1) -> o (ConstantTime 12) $ LD16 Immediate16 (ArgDirect16 HL)
    (0,6,1) -> o (ConstantTime 12) $ LD16 Immediate16 ArgSP

    (0,1,1) -> o (ConstantTime 8) $ ADD16_HL $ ArgDirect16 BC
    (0,3,1) -> o (ConstantTime 8) $ ADD16_HL $ ArgDirect16 DE
    (0,5,1) -> o (ConstantTime 8) $ ADD16_HL $ ArgDirect16 HL
    (0,7,1) -> o (ConstantTime 8) $ ADD16_HL $ ArgSP

    (0,y@1,2) -> o (ConstantTime 8) $ LD (registerPointerArg y) (ArgDirect8 A)
    (0,y@3,2) -> o (ConstantTime 8) $ LD (registerPointerArg y) (ArgDirect8 A)
    (0,y@5,2) -> o (ConstantTime 8) $ LD (registerPointerArg y) (ArgDirect8 A)
    (0,y@7,2) -> o (ConstantTime 8) $ LD (registerPointerArg y) (ArgDirect8 A)

    (0,y@0,2) -> o (ConstantTime 8) $ LD (ArgDirect8 A) (registerPointerArg y)
    (0,y@2,2) -> o (ConstantTime 8) $ LD (ArgDirect8 A) (registerPointerArg y)
    (0,y@4,2) -> o (ConstantTime 8) $ LD (ArgDirect8 A) (registerPointerArg y)
    (0,y@6,2) -> o (ConstantTime 8) $ LD (ArgDirect8 A) (registerPointerArg y)

    (0,0,3) -> o (ConstantTime 8) $ INC16 $ ArgDirect16 BC
    (0,2,3) -> o (ConstantTime 8) $ INC16 $ ArgDirect16 DE
    (0,4,3) -> o (ConstantTime 8) $ INC16 $ ArgDirect16 HL
    (0,6,3) -> o (ConstantTime 8) $ INC16 $ ArgSP

    (0,1,3) -> o (ConstantTime 8) $ DEC16 $ ArgDirect16 BC
    (0,3,3) -> o (ConstantTime 8) $ DEC16 $ ArgDirect16 DE
    (0,5,3) -> o (ConstantTime 8) $ DEC16 $ ArgDirect16 HL
    (0,7,3) -> o (ConstantTime 8) $ DEC16 $ ArgSP

    (0,y,4) -> o (ConstantTime $ if y == 6 then 12 else 4) $ INC (basicRegisterArg y)
    (0,y,5) -> o (ConstantTime $ if y == 6 then 12 else 4) $ DEC (basicRegisterArg y)
    (0,y,6) -> o (ConstantTime $ if y == 6 then 12 else 8) $ LD Immediate8 (basicRegisterArg y)

    (0,0,7) -> o (ConstantTime 4) RLCA
    (0,1,7) -> o (ConstantTime 4) RRCA
    (0,2,7) -> o (ConstantTime 4) RLA
    (0,3,7) -> o (ConstantTime 4) RRA

    (0,4,7) -> o (ConstantTime 4) DAA
    (0,5,7) -> o (ConstantTime 4) CPL
    (0,6,7) -> o (ConstantTime 4) SCF
    (0,7,7) -> o (ConstantTime 4) CCF

    (1,6,6) -> o (ConstantTime 4) HALT
    (1,y,z) -> o (ConstantTime $ if y == 6 || z == 6 then 8 else 4) $ LD (basicRegisterArg z) (basicRegisterArg y)

    (2,y,z) -> o (ConstantTime $ if z == 6 then 8 else 4) (aluMnemonic y $ basicRegisterArg z)

    (3,4,0) -> o (ConstantTime 12) $ LD (ArgDirect8 A) ArgPointerImmFF
    (3,6,0) -> o (ConstantTime 12) $ LD ArgPointerImmFF (ArgDirect8 A)
    (3,5,0) -> o (ConstantTime 16) ADD16_SP
    (3,7,0) -> o (ConstantTime 12) LD16_SP_HL
    (3,y,0) -> o (VariableTime 8 20) $ RET (Just $ flag y)

    (3,0,1) -> o (ConstantTime 12) $ POP (ArgDirect16 BC)
    (3,2,1) -> o (ConstantTime 12) $ POP (ArgDirect16 DE)
    (3,4,1) -> o (ConstantTime 12) $ POP (ArgDirect16 HL)
    (3,6,1) -> o (ConstantTime 12) $ POP (ArgDirect16 AF)

    (3,1,1) -> o (ConstantTime 16) $ RET Nothing
    (3,3,1) -> o (ConstantTime 16) RETI
    (3,5,1) -> o (ConstantTime  4) $ JP Nothing (ArgDirect16 HL)
    (3,7,1) -> o (ConstantTime  8) $ LD16 (ArgDirect16 HL) ArgSP

    (3,4,2) -> o (ConstantTime 8)  $ LD (ArgDirect8 A) (ArgPointerRegFF C)
    (3,6,2) -> o (ConstantTime 8)  $ LD (ArgPointerRegFF C) (ArgDirect8 A)
    (3,5,2) -> o (ConstantTime 16) $ LD (ArgDirect8 A) ArgPointerImm8
    (3,7,2) -> o (ConstantTime 16) $ LD ArgPointerImm8 (ArgDirect8 A)
    (3,y,2) -> o (VariableTime 12 16) $ JP (Just $ flag y) Address

    (3,0,3) -> o (ConstantTime 16) (JP Nothing Address)
    (3,1,3) -> error "0xCB"

    (3,6,3) -> o (ConstantTime 4) DI
    (3,7,3) -> o (ConstantTime 4) EI

    (3,f@0,4) -> o (VariableTime 12 24) $ CALL (Just $ flag f) Address
    (3,f@1,4) -> o (VariableTime 12 24) $ CALL (Just $ flag f) Address
    (3,f@2,4) -> o (VariableTime 12 24) $ CALL (Just $ flag f) Address
    (3,f@3,4) -> o (VariableTime 12 24) $ CALL (Just $ flag f) Address

    (3,0,5) -> o (ConstantTime 16) $ PUSH (ArgDirect16 BC)
    (3,2,5) -> o (ConstantTime 16) $ PUSH (ArgDirect16 DE)
    (3,4,5) -> o (ConstantTime 16) $ PUSH (ArgDirect16 HL)
    (3,6,5) -> o (ConstantTime 16) $ PUSH (ArgDirect16 AF)
    (3,1,5) -> o (ConstantTime 24) $ CALL Nothing Address

    (3,y,6) -> o (ConstantTime 8)  $ aluMnemonic y Immediate8
    (3,y,7) -> o (ConstantTime 16) $ RST y
    _ -> error $ printf "unknown bytecode 0x%02x" b

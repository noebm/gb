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

data Arg = ArgDirect16 Reg16
         | ArgSP

         | Immediate16

         | Address
         | AddressRel

         | ArgPointerImm16
         deriving (Eq)

instance Show Arg where
  show arg = case arg of
    ArgDirect16 r -> show r
    ArgSP -> "SP"

    Immediate16 -> "d16"

    Address    -> "a16"
    AddressRel -> "r8"

    ArgPointerImm16   -> "(a16)"

data Flag = FlagZ | FlagC | FlagNZ | FlagNC
  deriving (Eq)

instance Show Flag where
  show FlagZ  = "Z"
  show FlagNZ = "NZ"
  show FlagC  = "C"
  show FlagNC = "NC"

data InstructionExpr
  = LD In8 Out8 -- from -> to
  | PUSH Arg | POP Arg

  | JP (Maybe Flag) Arg | JR (Maybe Flag) Arg
  | CALL (Maybe Flag) Arg | RET (Maybe Flag)

  | NOP | STOP | HALT

  | LD16 Arg Arg -- from -> to
  | LD16_SP_HL -- sp + imm8 = hl
  | INC16 Arg | DEC16 Arg
  | ADD16_HL Arg
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

data In8 = InReg8 Reg8 | InImm8 | InAddr8 Addr
  deriving Show
data Out8 = OutReg8 Reg8 | OutAddr8 Addr
  deriving Show
data Addr = AddrBC | AddrDE | AddrHL | AddrHLi | AddrHLd | AddrDirect | ZeroPage | ZeroPageC
  deriving Show

-- every output is also in input
outToIn :: Out8 -> In8
outToIn (OutReg8 r) = InReg8 r
outToIn (OutAddr8 addr) = InAddr8 addr

basicRegisterArg :: Word8 -> Either Reg8 Addr
basicRegisterArg w = case w of
  0 -> Left $ B
  1 -> Left $ C
  2 -> Left $ D
  3 -> Left $ E
  4 -> Left $ H
  5 -> Left $ L
  6 -> Right $ AddrHL
  7 -> Left $ A
  _ -> error "basicRegisterArg: this should not be possible"

registerPointerArg' :: Word8 -> Addr
registerPointerArg' y = case y .&. 0x6 of
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
        in o (ConstantTime $ if z == 6 then 16 else 8) (op (either OutReg8 OutAddr8 $ basicRegisterArg z))

      (1,y,z) -> o (ConstantTime $ if z == 6 then 12 else 8) (BIT y $ either InReg8 InAddr8 $ basicRegisterArg z)
      (2,y,z) -> o (ConstantTime $ if z == 6 then 16 else 8) (RES y $ either OutReg8 OutAddr8 $ basicRegisterArg z)
      (3,y,z) -> o (ConstantTime $ if z == 6 then 16 else 8) (SET y $ either OutReg8 OutAddr8 $ basicRegisterArg z)
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

    (0,y@1,2) -> o (ConstantTime 8) $ LD (InAddr8 $ registerPointerArg' y) (OutReg8 A)
    (0,y@3,2) -> o (ConstantTime 8) $ LD (InAddr8 $ registerPointerArg' y) (OutReg8 A)
    (0,y@5,2) -> o (ConstantTime 8) $ LD (InAddr8 $ registerPointerArg' y) (OutReg8 A)
    (0,y@7,2) -> o (ConstantTime 8) $ LD (InAddr8 $ registerPointerArg' y) (OutReg8 A)

    (0,y@0,2) -> o (ConstantTime 8) $ LD (InReg8 A) (OutAddr8 $ registerPointerArg' y)
    (0,y@2,2) -> o (ConstantTime 8) $ LD (InReg8 A) (OutAddr8 $ registerPointerArg' y)
    (0,y@4,2) -> o (ConstantTime 8) $ LD (InReg8 A) (OutAddr8 $ registerPointerArg' y)
    (0,y@6,2) -> o (ConstantTime 8) $ LD (InReg8 A) (OutAddr8 $ registerPointerArg' y)

    (0,0,3) -> o (ConstantTime 8) $ INC16 $ ArgDirect16 BC
    (0,2,3) -> o (ConstantTime 8) $ INC16 $ ArgDirect16 DE
    (0,4,3) -> o (ConstantTime 8) $ INC16 $ ArgDirect16 HL
    (0,6,3) -> o (ConstantTime 8) $ INC16 $ ArgSP

    (0,1,3) -> o (ConstantTime 8) $ DEC16 $ ArgDirect16 BC
    (0,3,3) -> o (ConstantTime 8) $ DEC16 $ ArgDirect16 DE
    (0,5,3) -> o (ConstantTime 8) $ DEC16 $ ArgDirect16 HL
    (0,7,3) -> o (ConstantTime 8) $ DEC16 $ ArgSP

    (0,y,4) -> o (ConstantTime $ if y == 6 then 12 else 4) $ INC (either OutReg8 OutAddr8 $ basicRegisterArg y)
    (0,y,5) -> o (ConstantTime $ if y == 6 then 12 else 4) $ DEC (either OutReg8 OutAddr8 $ basicRegisterArg y)
    (0,y,6) -> o (ConstantTime $ if y == 6 then 12 else 8) $ LD InImm8 (either OutReg8 OutAddr8 $ basicRegisterArg y)

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
      $ LD (either InReg8 InAddr8 $ basicRegisterArg z) (either OutReg8 OutAddr8 $ basicRegisterArg y)

    (2,y,z) -> o (ConstantTime $ if z == 6 then 8 else 4) (aluMnemonic y $ either InReg8 InAddr8 $ basicRegisterArg z)

    (3,4,0) -> o (ConstantTime 12) $ LD (InReg8 A) (OutAddr8 ZeroPage)
    (3,6,0) -> o (ConstantTime 12) $ LD (InAddr8 ZeroPage) (OutReg8 A)
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

    (3,4,2) -> o (ConstantTime 8)  $ LD (InReg8 A) (OutAddr8 ZeroPageC)
    (3,6,2) -> o (ConstantTime 8)  $ LD (InAddr8 ZeroPageC) (OutReg8 A)
    (3,5,2) -> o (ConstantTime 16) $ LD (InReg8 A) (OutAddr8 AddrDirect)
    (3,7,2) -> o (ConstantTime 16) $ LD (InAddr8 AddrDirect) (OutReg8 A)
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

    (3,y,6) -> o (ConstantTime 8)  $ aluMnemonic y InImm8
    (3,y,7) -> o (ConstantTime 16) $ RST y
    _ -> error $ printf "unknown bytecode 0x%02x" b

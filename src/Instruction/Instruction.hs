module Instruction.Instruction where

import MonadEmulator
import Instruction

import Data.Traversable

import Text.Printf
import Data.Word
import Data.Bits

data Mnemonic
  = LD
  | PUSH | POP

  | JP | JR | CALL | RET
  | NOP | STOP | HALT

  | INC | DEC
  | ADD | SUB | ADC | SBC
  | CP
  | DI | EI | RETI | RST

  | AND | OR | XOR
  | RLCA | RRCA | RLA | RRA
  | RLC | RRC | RL | RR
  | SLA | SRA | SRL | SWAP
  | BIT | SET | RES

  | DAA | CPL | SCF | CCF
  deriving (Show, Eq)

data Arg = ArgDirect8 Reg8
         | ArgDirect16 Reg16

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

         | ArgFlag Flag
         | ArgByteCode Word8 -- extract argument data embedded in instruction

instance Show Arg where
  show arg = case arg of
    ArgDirect8 r -> show r
    ArgDirect16 r -> show r
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

    ArgFlag     f -> show f
    ArgByteCode b -> show b

showArgs :: [ Arg ] -> String
showArgs = showArgStructure . fmap show

showArgStructure :: [ String ] -> String
showArgStructure [] = ""
showArgStructure [ x ] = x
showArgStructure [ x, y ] = printf "%s, %s" x y
showArgStructure [ x, y, z ] = printf "%s, %s + %s" x y z
showArgStructure xs = show xs

argSize :: Arg -> Int
argSize Immediate16 = 2
argSize Immediate8  = 1
argSize Address     = 2
argSize AddressRel  = 1
argSize ArgPointerImm8  = 2
argSize ArgPointerImm16 = 2
argSize ArgPointerImmFF = 1
argSize _ = 0

opcodeSize :: Mnemonic -> Int
opcodeSize op
  | op `elem` [ RLC, RRC, RL, RR, SLA, SRA, SRL, SWAP, BIT, SET, RES ] = 2
  | otherwise = 1

instructionSize :: Instruction Arg -> Int
instructionSize (Instruction _ op args) = opcodeSize op + sum (argSize <$> args)

data Flag = FlagZ | FlagC | FlagNZ | FlagNC

instance Show Flag where
  show FlagZ  = "Z"
  show FlagNZ = "NZ"
  show FlagC  = "C"
  show FlagNC = "NC"

data Instruction a = Instruction !Word8 !Mnemonic ![ a ]

instance Functor Instruction where
  fmap f (Instruction code op args) = Instruction code op (fmap f args)

instance Foldable Instruction where
  foldMap = foldMapDefault

instance Traversable Instruction where
  traverse f (Instruction code op args) = Instruction code op <$> traverse f args

instance Show a => Show (Instruction a) where
  show (Instruction code mnemonic args)
    = printf "0x%02x - %s %s" code (show mnemonic) (showArgStructure $ show <$> args)

{-# INLINE arguments #-}
arguments :: Instruction a -> [ a ]
arguments (Instruction _ _ args) = args

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
aluMnemonic :: Word8 -> Mnemonic
aluMnemonic w = case w of
  0 -> ADD
  1 -> ADC
  2 -> SUB
  3 -> SBC
  4 -> AND
  5 -> XOR
  6 -> OR
  7 -> CP
  _ -> error "aluMnemonic: invalid argument"

{-# INLINE flag #-}
flag :: Word8 -> Flag
flag w = case w of
  0 -> FlagNZ
  1 -> FlagZ
  2 -> FlagNC
  3 -> FlagC
  _ -> error "flag: invalid argument"


parseInstructionM :: Monad m => m Word8 -> m (Instruction Arg)
parseInstructionM get = do
  b <- get
  if b == 0xCB
    then parseExtendedInstruction <$> get
    else return $ parseInstruction b

parseExtendedInstruction :: Word8 -> Instruction Arg
parseExtendedInstruction b =
  let {-# INLINE o #-}
      o = Instruction b
  in
    case byteCodeDecompose b of
      (0,y,z) ->
        let op = case y of { 0 -> RLC ; 1 -> RRC ; 2 -> RL; 3 -> RR; 4 -> SLA; 5 -> SRA; 6 -> SWAP; 7 -> SRL }
        in o op [ basicRegisterArg z ]

      (1,y,z) -> o BIT [ ArgByteCode y, basicRegisterArg z ]
      (2,y,z) -> o RES [ ArgByteCode y, basicRegisterArg z ]
      (3,y,z) -> o SET [ ArgByteCode y, basicRegisterArg z ]
      _ -> error $ printf "unknown bytecode 0x%02x" b

parseInstruction :: Word8 -> Instruction Arg
parseInstruction b =

  let {-# INLINE o #-}
      o = Instruction b
  in
    case byteCodeDecompose b of

    (0,0,0) -> o  NOP []
    (0,2,0) -> o STOP []
    (0,1,0) -> o LD   [ ArgPointerImm16, ArgDirect16 SP ]
    (0,3,0) -> o JR   [ AddressRel ]
    (0,y,0) -> o JR   [ ArgFlag $! flag (y .&. 0x3), AddressRel ]

    (0,0,1) -> o LD [ ArgDirect16 BC, Immediate16 ]
    (0,2,1) -> o LD [ ArgDirect16 DE, Immediate16 ]
    (0,4,1) -> o LD [ ArgDirect16 HL, Immediate16 ]
    (0,6,1) -> o LD [ ArgDirect16 SP, Immediate16 ]

    (0,1,1) -> o ADD [ ArgDirect16 HL, ArgDirect16 BC]
    (0,3,1) -> o ADD [ ArgDirect16 HL, ArgDirect16 DE]
    (0,5,1) -> o ADD [ ArgDirect16 HL, ArgDirect16 HL]
    (0,7,1) -> o ADD [ ArgDirect16 HL, ArgDirect16 SP]

    (0,y@1,2) -> o LD [ ArgDirect8 A, registerPointerArg y ]
    (0,y@3,2) -> o LD [ ArgDirect8 A, registerPointerArg y ]
    (0,y@5,2) -> o LD [ ArgDirect8 A, registerPointerArg y ]
    (0,y@7,2) -> o LD [ ArgDirect8 A, registerPointerArg y ]

    (0,y@0,2) -> o LD [ registerPointerArg y, ArgDirect8 A ]
    (0,y@2,2) -> o LD [ registerPointerArg y, ArgDirect8 A ]
    (0,y@4,2) -> o LD [ registerPointerArg y, ArgDirect8 A ]
    (0,y@6,2) -> o LD [ registerPointerArg y, ArgDirect8 A ]

    (0,0,3) -> o INC [ ArgDirect16 BC ]
    (0,2,3) -> o INC [ ArgDirect16 DE ]
    (0,4,3) -> o INC [ ArgDirect16 HL ]
    (0,6,3) -> o INC [ ArgDirect16 SP ]

    (0,1,3) -> o DEC [ ArgDirect16 BC ]
    (0,3,3) -> o DEC [ ArgDirect16 DE ]
    (0,5,3) -> o DEC [ ArgDirect16 HL ]
    (0,7,3) -> o DEC [ ArgDirect16 SP ]

    (0,y,4) -> o INC [ basicRegisterArg y ]
    (0,y,5) -> o DEC [ basicRegisterArg y ]
    (0,y,6) -> o LD  [ basicRegisterArg y, Immediate8 ]

    (0,0,7) -> o RLCA []
    (0,1,7) -> o RRCA []
    (0,2,7) -> o RLA  []
    (0,3,7) -> o RRA  []

    (0,4,7) -> o DAA []
    (0,5,7) -> o CPL []
    (0,6,7) -> o SCF []
    (0,7,7) -> o CCF []

    (1,6,6) -> o HALT []
    (1,y,z) -> o LD   [ basicRegisterArg y, basicRegisterArg z ]

    (2,y,z) -> o (aluMnemonic y) [ basicRegisterArg z ]

    (3,4,0) -> o LD  [ ArgPointerImmFF, ArgDirect8 A ]
    (3,6,0) -> o LD  [ ArgDirect8 A, ArgPointerImmFF ]
    (3,5,0) -> o ADD [ ArgDirect16 SP, AddressRel ]
    (3,7,0) -> o LD  [ ArgDirect16 HL, ArgDirect16 SP, AddressRel ]
    (3,y,0) -> o RET [ ArgFlag $! flag y ]

    (3,0,1) -> o POP [ ArgDirect16 BC ]
    (3,2,1) -> o POP [ ArgDirect16 DE ]
    (3,4,1) -> o POP [ ArgDirect16 HL ]
    (3,6,1) -> o POP [ ArgDirect16 AF ]

    (3,1,1) -> o RET  []
    (3,3,1) -> o RETI []
    (3,5,1) -> o JP   [ ArgDirect16 HL ]
    (3,7,1) -> o ADD  [ ArgDirect16 SP, ArgDirect16 HL ]

    (3,4,2) -> o LD [ ArgPointerRegFF C, ArgDirect8 A ]
    (3,6,2) -> o LD [ ArgDirect8 A, ArgPointerRegFF C ]
    (3,5,2) -> o LD [ ArgPointerImm8, ArgDirect8 A ]
    (3,7,2) -> o LD [ ArgDirect8 A, ArgPointerImm8 ]
    (3,y,2) -> o JP [ ArgFlag $! flag y, Address ]

    (3,0,3) -> o JP [ Address ]
    (3,1,3) -> error "0xCB"

    (3,6,3) -> o DI []
    (3,7,3) -> o EI []

    (3,f@0,4) -> o CALL [ArgFlag $! flag f, Address ]
    (3,f@1,4) -> o CALL [ArgFlag $! flag f, Address ]
    (3,f@2,4) -> o CALL [ArgFlag $! flag f, Address ]
    (3,f@3,4) -> o CALL [ArgFlag $! flag f, Address ]

    (3,0,5) -> o PUSH [ ArgDirect16 BC ]
    (3,2,5) -> o PUSH [ ArgDirect16 DE ]
    (3,4,5) -> o PUSH [ ArgDirect16 HL ]
    (3,6,5) -> o PUSH [ ArgDirect16 AF ]
    (3,1,5) -> o CALL [ Address ]

    (3,y,6) -> o (aluMnemonic y) [Immediate8]
    (3,y,7) -> o RST [ ArgByteCode y ]
    _ -> error $ printf "unknown bytecode 0x%02x" b
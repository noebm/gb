module OpCode where

import MonadEmulator
import Instruction

import Text.Printf
import Data.Word
import Data.Bits

-- byteCodeDecompose :: Word8 -> (Word8, Word8, Word8)
-- byteCodeDecompose b =
--   ((b `shiftR` 6) .&. 0x3, (b `shiftR` 3) .&. 0x7, b .&. 0x7)

data Mnemonic
  = LD
  | PUSH | POP

  | INC | DEC
  | JP | JR | CALL | RET

  | ADD | SUB | ADC | SBC
  | CP

  | NOP | STOP | HALT
  | DI | EI | RETI | RST

  | AND | OR | XOR
  | RLC | RRC | RL | RR
  | SLA | SRA | SRL | SWAP
  | BIT | SET | RES

  | DAA | CPL | SCF | CCF
  deriving (Show)

data Arg = ArgDirect8 Reg8
         | ArgDirect16 Reg16

         | Immediate8
         | Immediate16

         | Address
         | AddressFF
         | AddressRel

         | ArgPointerRegFF Reg8
         | ArgPointerReg Reg16
         | ArgPointerHLi
         | ArgPointerHLd
  deriving (Show)

argSize :: Arg -> Int
argSize Immediate16 = 2
argSize Immediate8  = 1
argSize Address     = 2
argSize AddressFF   = 1
argSize AddressRel  = 1
argSize _ = 0

data Flag = FlagZ | FlagC | FlagNZ | FlagNC
  deriving (Show)

data Instruction = Instruction !Word8 !Mnemonic !(Maybe Flag) ![ Arg ]
  deriving (Show)

arguments :: Instruction -> [ Arg ]
arguments (Instruction _ _ _ args) = args

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
        let op = case y of { 0 -> RLC ; 1 -> RRC ; 2 -> RL; 3 -> RR; 4 -> SLA; 5 -> SRA; 6 -> SWAP; 7 -> SRL }
        in o op Nothing [ basicRegisterArg z ]

      (1,y,z) -> o BIT Nothing [ basicRegisterArg z ]
      (2,y,z) -> o RES Nothing [ basicRegisterArg z ]
      (3,y,z) -> o SET Nothing [ basicRegisterArg z ]
      (x,y,z) -> error $ printf "unknown bytecode 0x%02x" b

parseInstruction :: Word8 -> Instruction
parseInstruction b =

  let {-# INLINE o #-}
      o = Instruction b
  in
    case byteCodeDecompose b of

    (0,0,0) -> o  NOP Nothing []
    (0,2,0) -> o STOP Nothing []
    (0,1,0) -> o LD   Nothing [ Address, ArgDirect16 SP ]
    (0,3,0) -> o JP   Nothing [ AddressRel ]
    (0,y,0) -> o JP   (Just $! flag y) [ AddressRel ]

    (0,0,1) -> o LD Nothing [ ArgDirect16 BC, Immediate16 ]
    (0,2,1) -> o LD Nothing [ ArgDirect16 DE, Immediate16 ]
    (0,4,1) -> o LD Nothing [ ArgDirect16 HL, Immediate16 ]
    (0,6,1) -> o LD Nothing [ ArgDirect16 SP, Immediate16 ]

    (0,1,1) -> o ADD Nothing [ ArgDirect16 HL, ArgDirect16 BC]
    (0,3,1) -> o ADD Nothing [ ArgDirect16 HL, ArgDirect16 DE]
    (0,5,1) -> o ADD Nothing [ ArgDirect16 HL, ArgDirect16 HL]
    (0,7,1) -> o ADD Nothing [ ArgDirect16 HL, ArgDirect16 SP]

    (0,y@0,2) -> o LD Nothing [ ArgDirect8 A, registerPointerArg y ]
    (0,y@2,2) -> o LD Nothing [ ArgDirect8 A, registerPointerArg y ]
    (0,y@4,2) -> o LD Nothing [ ArgDirect8 A, registerPointerArg y ]
    (0,y@6,2) -> o LD Nothing [ ArgDirect8 A, registerPointerArg y ]

    (0,y@1,2) -> o LD Nothing [ registerPointerArg y, ArgDirect8 A ]
    (0,y@3,2) -> o LD Nothing [ registerPointerArg y, ArgDirect8 A ]
    (0,y@5,2) -> o LD Nothing [ registerPointerArg y, ArgDirect8 A ]
    (0,y@7,2) -> o LD Nothing [ registerPointerArg y, ArgDirect8 A ]

    (0,0,3) -> o INC Nothing [ ArgDirect16 BC ]
    (0,2,3) -> o INC Nothing [ ArgDirect16 DE ]
    (0,4,3) -> o INC Nothing [ ArgDirect16 HL ]
    (0,6,3) -> o INC Nothing [ ArgDirect16 SP ]

    (0,1,3) -> o DEC Nothing [ ArgDirect16 BC ]
    (0,3,3) -> o DEC Nothing [ ArgDirect16 DE ]
    (0,5,3) -> o DEC Nothing [ ArgDirect16 HL ]
    (0,7,3) -> o DEC Nothing [ ArgDirect16 SP ]

    (0,y,4) -> o INC Nothing [ basicRegisterArg y ]
    (0,y,5) -> o DEC Nothing [ basicRegisterArg y ]
    (0,y,6) -> o LD Nothing  [ basicRegisterArg y, Immediate8 ]

    (0,0,7) -> o RLC Nothing [ ArgDirect8 A ]
    (0,1,7) -> o RRC Nothing [ ArgDirect8 A ]
    (0,2,7) -> o RL  Nothing [ ArgDirect8 A ]
    (0,3,7) -> o RR  Nothing [ ArgDirect8 A ]

    (0,4,7) -> o DAA Nothing []
    (0,5,7) -> o CPL Nothing []
    (0,6,7) -> o SCF Nothing []
    (0,7,7) -> o CCF Nothing []

    (1,6,6) -> o HALT Nothing []
    (1,y,z) -> o LD   Nothing [ basicRegisterArg y, basicRegisterArg z ]

    (2,y,z) -> o (aluMnemonic y) Nothing [ basicRegisterArg z ]

    (3,4,0) -> o LD  Nothing [ AddressFF, ArgDirect8 A ]
    (3,6,0) -> o LD  Nothing [ ArgDirect8 A, AddressFF ]
    (3,5,0) -> o ADD Nothing [ ArgDirect16 SP, AddressRel ]
    (3,7,0) -> o LD  Nothing [ ArgDirect16 HL, ArgDirect16 SP, AddressRel ]
    (3,y,0) -> o RET (Just $! flag y) []

    (3,0,1) -> o POP Nothing [ ArgDirect16 BC ]
    (3,2,1) -> o POP Nothing [ ArgDirect16 DE ]
    (3,4,1) -> o POP Nothing [ ArgDirect16 HL ]
    (3,6,1) -> o POP Nothing [ ArgDirect16 AF ]

    (3,1,1) -> o RET  Nothing []
    (3,3,1) -> o RETI Nothing []
    (3,5,1) -> o JP   Nothing [ ArgPointerReg HL ]
    (3,7,1) -> o ADD  Nothing [ ArgDirect16 SP, ArgDirect16 HL ]

    (3,4,2) -> o LD Nothing [ ArgPointerRegFF C, ArgDirect8 A ]
    (3,6,2) -> o LD Nothing [ ArgDirect8 A, ArgPointerRegFF C ]
    (3,5,2) -> o LD Nothing [ Address, ArgDirect8 A ]
    (3,7,2) -> o LD Nothing [ ArgDirect8 A, Address ]
    (3,y,2) -> o JP (Just $! flag y) [ Address ]

    (3,0,3) -> o JP Nothing [ Address ]
    (3,1,3) -> error "0xCB"

    (3,6,3) -> o DI Nothing []
    (3,7,3) -> o EI Nothing []

    (3,0,4) -> o CALL (Just $! flag 0) []
    (3,1,4) -> o CALL (Just $! flag 1) []
    (3,2,4) -> o CALL (Just $! flag 2) []
    (3,3,4) -> o CALL (Just $! flag 3) []

    (3,0,5) -> o PUSH Nothing [ ArgDirect16 BC ]
    (3,2,5) -> o PUSH Nothing [ ArgDirect16 DE ]
    (3,4,5) -> o PUSH Nothing [ ArgDirect16 HL ]
    (3,6,5) -> o PUSH Nothing [ ArgDirect16 AF ]
    (3,1,5) -> o CALL Nothing []

    (3,y,6) -> o (aluMnemonic y) Nothing [Immediate8]
    (3,_,7) -> o RST Nothing [] -- infer argument from bytecode
      -- error "z = 3"
    (x,y,z) -> error $ printf "unknown bytecode 0x%02x" b

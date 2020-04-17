module Instruction.Instruction
  ( InstructionExpr (..)
  , Instruction (..)
  , instructionExpr
  , instructionBranch
  , instructionFlag
  , parseInstructionM
  )
where

import Instruction.InOut
import Instruction.Flag

import Text.Printf
import Data.Word
import Data.Bits

import MonadEmulator.Operations

import MonadEmulator.EmulatorT

-- | decompose byte to xxyyyzzz
byteCodeDecompose :: Word8 -> (Word8, Word8, Word8)
byteCodeDecompose b =
  ((b `shiftR` 6) .&. 0x3, (b `shiftR` 3) .&. 0x7, b .&. 0x7)

data InstructionExpr
  = LD In8 Out8 -- from -> to
  | PUSH Reg16 | POP Reg16

  | JP Addr | JR
  | CALL | RET

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

data Instruction exp f a
  = InstructionNode a exp
  | InstructionBranch a a f exp
  deriving Show

instance Functor (Instruction exp f) where
  fmap f (InstructionNode x e) = InstructionNode (f x) e
  fmap f (InstructionBranch x y flag e) = InstructionBranch (f x) (f y) flag e

instructionExpr :: Instruction exp f a -> exp
instructionExpr (InstructionNode _ op) = op
instructionExpr (InstructionBranch _ _ _ op) = op

instructionBranch :: Instruction exp Bool a -> a
instructionBranch (InstructionNode t _) = t
instructionBranch (InstructionBranch tdef tbranch f _) = if f then tbranch else tdef

instructionFlag :: Instruction exp f a -> Maybe f
instructionFlag (InstructionNode _ _) = Nothing
instructionFlag (InstructionBranch _ _ f _) = Just f

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

argOut16 :: Word8 -> Out16
argOut16 w = case w .&. 0x6 of
  0 -> OutReg16 BC
  2 -> OutReg16 DE
  4 -> OutReg16 HL
  6 -> OutSP
  _ -> error "impossible"

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

{-# SPECIALIZE parseInstructionM :: Word8 -> Emulator (Instruction InstructionExpr Flag Word) #-}
parseInstructionM :: MonadEmulator m => Word8 -> m (Instruction InstructionExpr Flag Word)
parseInstructionM 0xCB = parseExtendedInstruction <$> byte
parseInstructionM b = return $ parseInstruction b

parseExtendedInstruction :: Word8 -> Instruction InstructionExpr f Word
parseExtendedInstruction b =
  let {-# INLINE o #-}
      o = InstructionNode
  in
    case byteCodeDecompose b of
      (0,y,z) ->
        let op = case y of { 0 -> RLC ; 1 -> RRC ; 2 -> RL; 3 -> RR; 4 -> SLA; 5 -> SRA; 6 -> SWAP; _ -> SRL }
        in o (if z == 6 then 16 else 8) (op (basicRegisterArg z))

      (1,y,z) -> o (if z == 6 then 12 else 8) (BIT y $ outToIn $ basicRegisterArg z)
      (2,y,z) -> o (if z == 6 then 16 else 8) (RES y $ basicRegisterArg z)
      (3,y,z) -> o (if z == 6 then 16 else 8) (SET y $ basicRegisterArg z)
      _ -> error $ printf "unknown bytecode 0x%02x" b

parseInstruction :: Word8 -> Instruction InstructionExpr Flag Word
parseInstruction b =

  let {-# INLINE o #-}
      o = InstructionNode
  in
    case byteCodeDecompose b of

    (0,0,0) -> o 4 NOP
    (0,2,0) -> o 4 STOP
    (0,1,0) -> o 20 (LD16 InSP OutImmAddr16)
    (0,3,0) -> o 12 JR
    (0,y,0) -> InstructionBranch 8 12 (flag (y .&. 0x3)) JR

    (0,y,1) | y `testBit` 0 -> o 8 $ ADD16_HL $ out16ToIn16 (argOut16 y)
            | otherwise     -> o 12 $ LD16 InImm16 (argOut16 y)

    (0,y,2) | y `testBit` 0 -> o 8 $ LD (InAddr8 $ registerPointerArg y) (OutReg8 A)
            | otherwise     -> o 8 $ LD (InReg8 A) (OutAddr8 $ registerPointerArg y)

    (0,y,3) | y `testBit` 0 -> o 8 $ DEC16 $ argOut16 y
            | otherwise     -> o 8 $ INC16 $ argOut16 y

    (0,y,4) -> o (if y == 6 then 12 else 4) $ INC (basicRegisterArg y)
    (0,y,5) -> o (if y == 6 then 12 else 4) $ DEC (basicRegisterArg y)
    (0,y,6) -> o (if y == 6 then 12 else 8) $ LD InImm8 (basicRegisterArg y)

    (0,0,7) -> o 4 RLCA
    (0,1,7) -> o 4 RRCA
    (0,2,7) -> o 4 RLA
    (0,3,7) -> o 4 RRA

    (0,4,7) -> o 4 DAA
    (0,5,7) -> o 4 CPL
    (0,6,7) -> o 4 SCF
    (0,7,7) -> o 4 CCF

    (1,6,6) -> o 4 HALT
    (1,y,z) -> o (if y == 6 || z == 6 then 8 else 4)
      $ LD (outToIn $ basicRegisterArg z) (basicRegisterArg y)

    (2,y,z) -> o (if z == 6 then 8 else 4) (aluMnemonic y $ outToIn $ basicRegisterArg z)

    (3,4,0) -> o 12 $ LD (InReg8 A) (OutAddr8 ZeroPage)
    (3,6,0) -> o 12 $ LD (InAddr8 ZeroPage) (OutReg8 A)
    (3,5,0) -> o 16 ADD16_SP
    (3,7,0) -> o 12 LD16_SP_HL
    (3,y,0) -> InstructionBranch 8 20 (flag y) $ RET

    (3,0,1) -> o 12 $ POP BC
    (3,2,1) -> o 12 $ POP DE
    (3,4,1) -> o 12 $ POP HL
    (3,6,1) -> o 12 $ POP AF

    (3,1,1) -> o 16 $ RET
    (3,3,1) -> o 16 RETI
    (3,5,1) -> o  4 $ JP AddrHL
    (3,7,1) -> o  8 $ LD16 (InReg16 HL) OutSP

    (3,4,2) -> o 8  $ LD (InReg8 A) (OutAddr8 ZeroPageC)
    (3,6,2) -> o 8  $ LD (InAddr8 ZeroPageC) (OutReg8 A)
    (3,5,2) -> o 16 $ LD (InReg8 A) (OutAddr8 AddrDirect)
    (3,7,2) -> o 16 $ LD (InAddr8 AddrDirect) (OutReg8 A)
    (3,y,2) -> InstructionBranch 12 16 (flag y) $ JP AddrDirect

    (3,0,3) -> o 16 (JP AddrDirect)
    (3,1,3) -> error "0xCB"

    (3,6,3) -> o 4 DI
    (3,7,3) -> o 4 EI

    (3,f,4) | f < 4 -> InstructionBranch 12 24 (flag f) $ CALL

    (3,0,5) -> o 16 $ PUSH BC
    (3,2,5) -> o 16 $ PUSH DE
    (3,4,5) -> o 16 $ PUSH HL
    (3,6,5) -> o 16 $ PUSH AF
    (3,1,5) -> o 24 $ CALL

    (3,y,6) -> o 8  $ aluMnemonic y InImm8
    (3,y,7) -> o 16 $ RST y
    _ -> error $ printf "unknown bytecode 0x%02x" b

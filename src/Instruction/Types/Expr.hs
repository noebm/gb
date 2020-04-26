module Instruction.Types.Expr where

import Instruction.Types.Address
import Instruction.Types.Readable
import Instruction.Types.Writable

import Data.Word
import Data.Int

import CPU.Registers

import Text.Printf

data Expr
  = LD Readable8 Writable8 -- from -> to
  | PUSH Reg16 | POP Reg16

  | JP Addr | JR !Int8
  | CALL !Word16 | RET

  | NOP | STOP | HALT

  | LD16 Readable16 Writable16 -- from -> to
  | LD16_SP_HL !Int8 -- sp + imm8 = hl
  | INC16 Writable16 | DEC16 Writable16
  | ADD16_HL Readable16
  | ADD16_SP

  | INC Writable8 | DEC Writable8
  | ADD Readable8 | SUB Readable8 | ADC Readable8 | SBC Readable8
  | CP Readable8

  | DI | EI | RETI | RST Word8

  | AND Readable8 | OR Readable8 | XOR Readable8
  | RLCA | RRCA | RLA | RRA

  | RLC Writable8 | RRC Writable8 | RL Writable8 | RR Writable8
  | SLA Writable8 | SRA Writable8 | SRL Writable8 | SWAP Writable8

  | BIT Word8 Readable8 | SET Word8 Writable8 | RES Word8 Writable8

  | DAA | CPL | SCF | CCF
  deriving Show

exprASM :: Expr -> String
exprASM (CALL addr) = printf "CALL 0x%04x" addr
exprASM e = show e

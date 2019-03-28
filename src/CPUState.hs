{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module CPUState where

import Control.Lens
import Control.Lens.Unsound (lensProduct)

import Text.Printf

import Data.Word

import MonadEmulator

data CPUState = CPUState
  -- special registers
  { _regPC :: !Word16 -- program counter
  , _regSP :: !Word16 -- stack pointer
  -- normal registers
  , _regA :: !Word8
  , _regF :: !Word8 -- flags
  , _regB :: !Word8
  , _regC :: !Word8
  , _regD :: !Word8
  , _regE :: !Word8
  , _regH :: !Word8
  , _regL :: !Word8
  } deriving (Eq)

instance Show CPUState where
  show (CPUState pc sp a f b c d e h l) =
    printf "PC: %04x, SP: %04x, A: %02x, F: %02x, B: %02x, C: %02x, D: %02x, E: %02x, H: %02x, L: %02x" pc sp a f b c d e h l

newCPUState :: CPUState
newCPUState = CPUState 0 0 0 0 0 0 0 0 0 0

makeLenses ''CPUState

reg8lens :: Reg8 -> Lens' CPUState Word8
reg8lens A = regA
reg8lens B = regB
reg8lens C = regC
reg8lens D = regD
reg8lens E = regE
reg8lens F = regF
reg8lens H = regH
reg8lens L = regL

reg16lens :: Reg16 -> Lens' CPUState Word16
reg16lens AF = lensProduct regA regF . word16
reg16lens BC = lensProduct regB regC . word16
reg16lens DE = lensProduct regD regE . word16
reg16lens HL = lensProduct regH regL . word16
reg16lens PC = regPC
reg16lens SP = regSP

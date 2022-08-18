module Disassemble
  ( disassemble
  , disassembleAt
  , showAddressMap
  ) where

import qualified Instruction.Disassembler      as D
import           Instruction.Instruction
import           Instruction.Types.Flag

import           MonadEmulator

import           Hardware.Cartridge.Rom

import qualified Data.Map                      as M
import           Data.Word
import           Text.Printf

import           Control.Monad.ST

config :: Rom -> EmulatorConfig
config = EmulatorConfig Nothing

disassemble :: Rom -> M.Map Word16 (Instruction Flag ())
disassemble rom = runST $ runEmulatorT (config rom) D.disassemble

disassembleAt :: Rom -> Word16 -> M.Map Word16 (Instruction Flag ())
disassembleAt rom addr =
  runST $ runEmulatorT (config rom) $ D.disassembleAt addr

showAddressMap :: M.Map Word16 (Instruction Flag ()) -> String
showAddressMap = unlines . M.foldMapWithKey go
  where go addr val = [printf "0x%04x: %s" addr (instructionASM val)]

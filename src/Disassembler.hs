module Disassembler where

import MonadEmulator
import Interpret
import OpCode
import Text.Printf

import Data.Traversable

import Data.Word
import Data.Int

data ArgData = ArgByte Word8 | ArgWord Word16

argData :: MonadEmulator m => Arg -> m (Maybe ArgData)
argData arg = case argSize arg of
  0 -> return Nothing
  1 -> Just . ArgByte <$> byte
  2 -> Just . ArgWord <$> word
  _ -> error "impossible argsize"

data DisassembledInstruction
  = DissassembledInstruction
  { address   :: Word16
  , mnemonic  :: Mnemonic
  , arguments :: [ (Arg , Maybe ArgData) ]
   }

disassemble :: MonadEmulator m => m DisassembledInstruction
disassemble = do
  pc <- load16 (Register16 PC)
  Instruction _ op args <- parseInstructionM byte
  args' <- forM args $ \arg -> do
    darg <- argData arg
    return (arg , darg)
  return $ DissassembledInstruction pc op args'

instance Show DisassembledInstruction where
  show (DissassembledInstruction addr mne args)
    = printf "0x%04x: %s %s" addr (show mne) (showArgStructure $ disassembleArg <$> args)
    where
    disassembleArg (t , Nothing) = show t
    disassembleArg (t , Just d)
      | ArgByte b <- d = case t of
          Immediate8 -> printf "0x%02x" b
          AddressRel -> printf "0x%02x" (fromIntegral b :: Int8)
          ArgPointerImmFF -> printf "(0xFF%02x)" b
          _ -> error $ printf "%s has byte data" (show t)

      | ArgWord w <- d = case t of
          Immediate16 -> printf "0x%04x" w
          Address     -> printf "0x%04x" w
          ArgPointerImm8  -> printf "(0x%04x)" w
          ArgPointerImm16 -> printf "(0x%04x)" w
          _ -> error $ printf "%s has word data" (show t)


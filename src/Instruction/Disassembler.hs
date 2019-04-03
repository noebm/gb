module Instruction.Disassembler where

import Control.Monad.State

import MonadEmulator
import Instruction.Interpret
import Instruction.Instruction
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
  = DisassembledInstruction
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
  return $ DisassembledInstruction pc op args'

hasTargetAddress :: DisassembledInstruction -> Maybe Word16
hasTargetAddress (DisassembledInstruction addr op args) = case op of
  JP | (Address, Just (ArgWord addr')) : _ <- reverse args -> Just addr'
  JR | (AddressRel, Just (ArgByte r))  : _ <- reverse args -> Just $ addRelative addr (fromIntegral r)
  CALL | (Address, Just (ArgWord addr')) : _ <- reverse args -> Just addr'
  _ -> Nothing

changesControlFlow :: Mnemonic -> Bool
changesControlFlow op = op `elem` [ JP, JR, CALL, RET, RETI, RST, STOP, HALT ]

-- isConditional :: DisassembledInstruction -> ``
-- 
-- runDisassembler :: MonadEmulator m => m [ DisassembledInstruction ]
-- runDisassembler = (`execStateT` []) $ do
--   store16 (Register16 PC) 0
--   let parse = do
--         dis <- disassemble
--         modify' (dis:)
--         let DisassembledInstruction _ op _ = dis
--         if changesControlFlow op
--           then do
--           let addr = hasTargetAddress dis
--           
--           -- mapM_ (parse << load16 (Register16 PC))
--           else
--           parse
--   parse
--   return ()

instance Show DisassembledInstruction where
  show (DisassembledInstruction addr mne args)
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


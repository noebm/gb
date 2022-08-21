module Instruction.Disassembler
  ( disassemble
  , disassembleAt
  ) where

import qualified Data.Map                      as M
import           Data.Word

import           Instruction.Instruction
import           Instruction.Parser
import           Instruction.Types.Address
import           Instruction.Types.Flag

import           Data.Maybe                     ( fromMaybe )
import           MonadEmulator.Class
import           MonadEmulator.Operations

import           Control.Lens
import           Control.Monad.State

import           Data.Functor

-- parses instruction and returns possible addresses
step :: MonadEmulator m => m (Instruction Flag (Maybe Word16))
step = do
  instr <- parseInstructionM =<< byte
  jp    <- case instr ^. expr of
    JP   addr -> Just <$> getAddress addr -- XXX: handle non immediate addresses
    JR   addr -> Just . (`addRelative` addr) <$> loadPC
    CALL addr -> return (Just addr)
    RST  addr -> return $ Just (fromIntegral addr)
    _         -> return Nothing
  return $ instr & branch' .@~ (\flg -> guard flg *> jp)

build
  :: MonadEmulator m
  => Word16
  -> StateT (M.Map Word16 (Instruction Flag ())) m ()
build addr = do
  lift $ jump addr
  instr <- lift step
  pc    <- lift loadPC
  at addr ?= (instr $> ())
  let isUnconditionalRet =
        hasn't flag instr && (instr ^. expr == RET || instr ^. expr == RETI)
  unless isUnconditionalRet $ do

    -- quick fix for checking after a CALL op returns
    -- this is to work around using no stack
    -- otherwise a nonconditional CALL will just check the branch
    let instr' = case instr of
          InstructionNode addr (CALL addr') ->
            InstructionBranch Nothing addr () (CALL' addr')
          _ -> instr & flag .~ ()

    forOf_ branch' instr' $ \maddr' -> do
      let addr' = fromMaybe pc maddr'
      assignable <- use (at addr' . to (has _Nothing))
      when assignable $ build addr'

disassembleF
  :: (Foldable t, MonadEmulator m)
  => t Word16
  -> m (M.Map Word16 (Instruction Flag ()))
disassembleF addresses = execStateT (mapM_ build addresses) M.empty

-- | Basic disassembler
-- Generates instruction map for all interrupt addresses and the main entry point
disassemble :: MonadEmulator m => m (M.Map Word16 (Instruction Flag ()))
disassemble = disassembleF [0x40, 0x48, 0x50, 0x58, 0x60, 0x100]

-- | Starts disassembly at specified point
disassembleAt
  :: MonadEmulator m => Word16 -> m (M.Map Word16 (Instruction Flag ()))
disassembleAt = disassembleF . Identity

module Instruction.CodePath where

import Data.Word
import Control.Monad

import MonadEmulator
import Instruction.Instruction
import Instruction.Interpret
import Interrupt

data CodePath m = CodePath
  { entryAddress :: Word16
  , pathInstructions :: [ Instruction ]
  , executePath :: m ()
  }

-- getCodePathWithInterrupts :: MonadEmulator m => Int -> m (Maybe (CodePath m))
getCodePathWithInterrupts threshold = do
  entry <- load16 (Register16 PC)
  let skipOpcode (Instruction _ op _) =
        store16 (Register16 PC) . (+ fromIntegral (opcodeSize op)) =<< load16 (Register16 PC)
  let interruptRoutine i = do
        advCycles =<< enterInterrupt i
        let f = do
              i' <- parseInstructionM byte
              interpretM i'
              let Instruction _ op _ = i'
              when (op /= RETI) f
        f
  let manageInterrupts = mapM_ interruptRoutine =<< handleInterrupt
  let getOp = do
        i <- parseInstructionM byte
        let stmnt = interpretM i
        stmnt
        return (i , manageInterrupts >> skipOpcode i >> stmnt)
  let run = do
        (i, stmnt) <- getOp
        if isControlStatement i
          then return []
          else ((i , stmnt) :) <$> run
  stmnts <- run
  return $ do
    guard (length stmnts > threshold)
    let (is , runner) = unzip stmnts
    return $ CodePath entry is (sequence_ runner)



{- execute a path and return the actions up to the next control statement -}
getCodePath :: MonadEmulator m => Int -> m (Maybe (CodePath m))
getCodePath threshold = do
  entry <- load16 (Register16 PC)
  let skipOpcode (Instruction _ op _) =
        store16 (Register16 PC) . (+ fromIntegral (opcodeSize op)) =<< load16 (Register16 PC)
  let getOp = do
        i <- parseInstructionM byte
        let stmnt = interpretM i
        stmnt
        return (i , skipOpcode i >> stmnt)
  let run = do
        (i, stmnt) <- getOp
        if isControlStatement i
          then return []
          else ((i , stmnt) :) <$> run
  stmnts <- run
  return $ do
    guard (length stmnts > threshold)
    let (is , runner) = unzip stmnts
    return $ CodePath entry is (sequence_ runner)

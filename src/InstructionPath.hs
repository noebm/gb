module InstructionPath where

import Data.Word
import Control.Monad

import MonadEmulator
import OpCode
import Interpret

data CodePath m = CodePath
  { entryAddress :: Word16
  , pathInstructions :: [ Instruction ]
  , executePath :: m ()
  }

{- execute a path and return the actions up to the next control statement -}
getCodePath :: MonadEmulator m => Int -> m (Maybe (CodePath m))
getCodePath threshold = do
  entry <- load16 (Register16 PC)
  let skipOpcode i =
        store16 (Register16 PC) . (+ fromIntegral (opcodeSize i)) =<< load16 (Register16 PC)
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

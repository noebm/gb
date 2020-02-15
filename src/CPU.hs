module CPU
  ( Step (..),
    initCPUStep
  )
where

import Control.Applicative
import Control.Monad

import MonadEmulator

import Instruction.Instruction
import Instruction.Interpret

import GB

import Utilities.Step

{-# SPECIALIZE initCPUStep :: Step (GB IO) Word #-}
initCPUStep :: (MonadEmulator m) => Step m Word
initCPUStep = stepsFromLoop run (Running <$> byte)

run :: (MonadEmulator m) => StepInfo -> m (Word, StepInfo)
run info = case info of
  PendingInterrupt i -> do
    serviceInterrupt i
    op <- Running <$> byte
    return (20, op)
  Halt -> do
    i <- anyInterrupts
    ime <- getIME
    out <- maybe (return Halt) id
      $   ((pure . PendingInterrupt) <$> i <* guard ime)
      <|> ((Running <$> byte) <$ i)
    return (4, out)
  Running op -> interpretM =<< parseInstructionM op

stepCPU :: (MonadEmulator m) => StepInfo -> m (Word, Step m Word)
stepCPU i = do
  (dt , i) <- run i
  return (dt, Step $ stepCPU i)


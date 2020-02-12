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

data Step m = Step { runStep :: m (Word , Step m) }

{-# SPECIALIZE initCPUStep :: Step (GB IO) #-}
initCPUStep :: (HardwareMonad m, MonadEmulator m) => Step m
initCPUStep = Step $ stepCPU (Running 0x00)

run :: (HardwareMonad m, MonadEmulator m) => StepInfo -> m (Word, StepInfo)
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

stepCPU :: (HardwareMonad m, MonadEmulator m) => StepInfo -> m (Word, Step m)
stepCPU i = do
  (dt , i) <- run i
  return (dt, Step $ stepCPU i)


module MonadEmulator
  ( MonadEmulator (..)

  , tickHardware
  , showRegisters

  , module CPU.Registers
  , module MonadEmulator.EmulatorT
  )
where

import Control.Monad

import Text.Printf

import Hardware.HardwareMonad
import MonadEmulator.Class
import MonadEmulator.EmulatorT
import CPU.Registers

showRegisters :: MonadEmulator m => m String
showRegisters = do
  let rs8 = [A, F, B, C, D, E, H, L]
  s1 <- forM rs8 $ \r -> do
    v <- loadReg r
    return $ show r ++ printf ": %02x " v
  s2 <- forM [("PC", loadPC), ("SP", loadSP)] $ \(name, get) -> do
    v <- get
    return $ name ++ printf ": %04x " v
  return $ concat s1 ++ concat s2


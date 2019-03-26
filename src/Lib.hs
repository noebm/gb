{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
where

import Control.Lens
import Control.Monad.State

import MonadEmulator
import Instruction

import CPUState
import GBState
import Memory

newtype GBT m a = GBT (StateT GBState m a)
  deriving (Functor, Applicative, Monad, MonadState GBState, MonadIO)

runGB :: Monad m => GBT m a -> GBState -> m a
runGB (GBT act) s = evalStateT act s

instance Monad m => MonadEmulator (GBT m) where
  -- store8 :: LoadStore8 -> Word8 -> m ()
  store8 (Register8 r) = assign (cpuState.reg8lens r)
  store8 (Addr8 addr)  = writeMemory addr

  load8 (Register8 r) = use (cpuState.reg8lens r)
  load8 (Addr8 addr)  = accessMemory addr

  store16 (Register16 r) dw = assign (cpuState.reg16lens r) dw
  store16 (Addr16 addr) dw = do
    let (hw , lw) = dw ^. from word16
    store8 (Addr8 addr) lw
    store8 (Addr8 $ addr + 1) hw

  load16 (Register16 r) = use (cpuState.reg16lens r)
  load16 (Addr16 addr) = do
    lw <- load8 (Addr8 addr)
    hw <- load8 (Addr8 $ addr + 1)
    return $ (hw , lw) ^. word16

  advCycles dt = timer += dt
  getCycles = use timer


someFunc :: IO ()
someFunc = do
  rom <- memoryBootRom
  let s = newGBState rom
  (`runGB` s) $ do
    forM_ [1..5] $ \ k -> do
      b <- immediate8
      liftIO . putStrLn $ "Instruction: " ++ hexbyte b
      advCycles =<< instruction b
      liftIO . print =<< use cpuState
  return ()

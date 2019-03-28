{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
where

import Control.Lens
import Control.Monad.State
import Text.Printf

import MonadEmulator
import Instruction

import CPUState
import GBState
import Memory (memoryBootRom, mmio)
import MMIO

newtype GBT m a = GBT (StateT GBState m a)
  deriving (Functor, Applicative, Monad, MonadState GBState, MonadIO)

runGB :: Monad m => GBT m a -> GBState -> m a
runGB (GBT act) = evalStateT act

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

  advCycles dt = do
    timer += dt
    -- reset timer after some reasonable amount of time
    let pow24 = 16777216 -- 2 ** 24
    timer %= \t -> if t >= pow24 then t - pow24 else t
  getCycles = use timer


interpret :: MonadIO m => Bool -> Word -> GBT m ()
interpret enablePrinting t = do
  b <- immediate8
  advCycles =<< instruction b
  t' <- getCycles
  let dt = t - t'
  m <- use $ memory.mmio
  m' <- execStateT (updateGPU dt) m
  assign (memory.mmio) m'
  when enablePrinting $ do
    pc <- use $ cpuState.regPC
    -- liftIO $ putStrLn $ printf "Instruction: 0x%02x / PC: 0x%04x" b pc
    if pc == 0xe9 then error "at 0xe9" else return ()
    -- ly <- use memory.mmio.ly
    -- liftIO . print =<< use cpuState
  interpret enablePrinting t'

someFunc :: IO ()
someFunc = do
  rom <- memoryBootRom
  let s = newGBState rom
  (`runGB` s) $ do
    interpret True 0
    -- forM_ [1..5] $ \ k -> do
    --   b <- immediate8
    --   liftIO . putStrLn $ "Instruction: " ++ hexbyte b
    --   advCycles =<< instruction b
    --   liftIO . print =<< use cpuState
  return ()

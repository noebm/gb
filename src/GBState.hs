{-# LANGUAGE TemplateHaskell, FlexibleContexts, NoMonomorphismRestriction #-}
module GBState where

import Control.Monad.State
import Control.Lens

import Data.Word
import CPUState

import qualified Memory as M

data GBState = GBState
  { _cpuState :: CPUState
  , _memory :: M.Memory
  , _timer :: Word
  }

newGBState :: M.Memory -> GBState
newGBState m = GBState newCPUState m 0

makeLenses ''GBState

accessMemory :: MonadState GBState m => Word16 -> m Word8
accessMemory addr = do
  m <- use memory
  M.accessMemory addr `evalStateT` m

writeMemory :: MonadState GBState m => Word16 -> Word8 -> m ()
writeMemory addr w = do
  m <- use memory
  m' <- M.writeMemory addr w `execStateT` m
  assign memory m'

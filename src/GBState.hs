{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
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
  deriving (Show)

newGBState :: M.Memory -> GBState
newGBState m = GBState newCPUState m 0

makeLenses ''GBState

accessMemory :: MonadState GBState m => Word16 -> m Word8
accessMemory addr = use (memory.singular (ix (fromIntegral addr)))

writeMemory :: MonadState GBState m => Word16 -> Word8 -> m ()
writeMemory addr = assign (memory.singular (ix (fromIntegral addr)))

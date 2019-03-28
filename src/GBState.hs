{-# LANGUAGE TemplateHaskell, FlexibleContexts, NoMonomorphismRestriction #-}
module GBState where

import Control.Monad.State
import Control.Lens
import Control.Monad.IO.Class

import Data.Word
import CPUState
import Graphics
import SDL.Video.Renderer
import SDL.Video

import qualified Memory as M

data GBState = GBState
  { _cpuState :: !CPUState
  , _memory :: M.Memory
  , _timer :: !Word
  , _graphics :: GraphicsContext
  }

newGBState :: MonadIO m => M.Memory -> m GBState
newGBState m = do
  x <- initializeGraphics
  return $ GBState newCPUState m 0 x

makeLenses ''GBState

accessMemory :: MonadState GBState m => Word16 -> m Word8
accessMemory addr = do
  m <- use memory
  (w , m') <- M.accessMemory addr `runStateT` m
  assign memory m'
  return w

writeMemory :: MonadState GBState m => Word16 -> Word8 -> m ()
writeMemory addr w = do
  m <- use memory
  m' <- M.writeMemory addr w `execStateT` m
  assign memory m'

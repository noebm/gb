{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances, TypeFamilies #-}
module GB
( MonadEmulator(..)
, GB
, runGB
, showRegisters
)
where

import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed.Mutable (MVector)
import Data.STRef
import Data.Word
import Text.Printf

import Control.Lens
import Control.Monad.ST
import Control.Monad.Reader

import MonadEmulator
import HardwareMonad

import CPU.Registers
import GPU.GPUState
import Interrupt.Interrupt
import Timer
import Cartridge.Cartridge
import Joypad

data GBState s = GBState
  { hiram :: MVector s Word8
  , ram   :: MVector s Word8

  , cpu :: MVector s Word8
  , stackpointer   :: STRef s Word16
  , programCounter :: STRef s Word16
  , ime :: STRef s Bool

  , shouldStop   :: STRef s Bool
  , isHalted     :: STRef s Bool

  , gbTimer     :: STRef s Timer
  , gbInterrupt :: STRef s InterruptState
  , gbGPU       :: STRef s GPUState
  , gbJoypad    :: STRef s JoypadState

  , gbCartridge   :: STRef s CartridgeState
  }

newtype GBT s m a = GBT (ReaderT (GBState s) m a)
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (GBT s m) where
  liftIO f = GBT $ liftIO f

type GB = GBT RealWorld

makeGBState :: CartridgeState -> ST s (GBState s)
makeGBState cart = do
  hiram <- V.replicate 0x7f 0x00
  ram <- V.replicate 0x2000 0x00
  cpu <- V.replicate 0x8 0x00
  GBState
    <$> pure hiram
    <*> pure ram
    <*> pure cpu
    <*> newSTRef 0x0000
    <*> newSTRef 0x0000
    <*> newSTRef False
    <*> newSTRef False
    <*> newSTRef False
    <*> newSTRef defaultTimer
    <*> newSTRef defaultInterruptState
    <*> newSTRef defaultGPUState
    <*> newSTRef defaultJoypadState
    <*> newSTRef cart

runGB :: MonadIO m => CartridgeState -> GB m a -> m a
runGB cart (GBT x) = do
  gbState <- liftIO $ stToIO $ makeGBState cart
  runReaderT x gbState

readState :: MonadIO m => (GBState s -> STRef RealWorld a) -> GBT s m a
readState  f   = GBT $ liftIO . stToIO . readSTRef =<< asks f

writeState :: MonadIO m => (GBState s -> STRef RealWorld a) -> a -> GBT s m ()
writeState f b = GBT $ liftIO . stToIO . (`writeSTRef` b) =<< asks f

{-# INLINE reg8index #-}
reg8index :: Reg8 -> Int
reg8index A = 0
reg8index F = 1
reg8index B = 2
reg8index C = 3
reg8index D = 4
reg8index E = 5
reg8index H = 6
reg8index L = 7

instance MonadIO m => HardwareMonad (GB m) where

  getTimer = readState  gbTimer
  putTimer = writeState gbTimer

  getGPU = readState  gbGPU
  putGPU = writeState gbGPU

  getInterrupt = readState  gbInterrupt
  putInterrupt = writeState gbInterrupt

  getJoypad = readState gbJoypad
  putJoypad = writeState gbJoypad

  getCartridge = readState gbCartridge
  putCartridge = writeState gbCartridge

  readRAM idx = GBT $ do
    ram <- asks ram
    liftIO $ V.read ram (fromIntegral idx)

  readHRAM idx = GBT $ do
    hiram <- asks hiram
    liftIO $ V.read hiram (fromIntegral idx)

  writeRAM idx b = GBT $ do
    ram <- asks ram
    liftIO $ V.write ram (fromIntegral idx) b

  writeHRAM idx b = GBT $ do
    hiram <- asks hiram
    liftIO $ V.write hiram (fromIntegral idx) b

instance MonadIO m => MonadEmulator (GB m) where

  storeReg r b = GBT $ do
    regs <- asks cpu
    liftIO $ V.write regs (reg8index r) b

  storeSP = writeState stackpointer
  storePC = writeState programCounter

  loadReg r = GBT $ do
    regs <- asks cpu
    liftIO $ V.read regs (reg8index r)

  loadSP = readState stackpointer
  loadPC = readState programCounter

  setStop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ writeSTRef s True

  stop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ readSTRef s

  getIME = readState ime
  setIME = writeState ime

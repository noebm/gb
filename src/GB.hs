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

import Control.Monad.ST
import Control.Monad.Reader

import MonadEmulator
import Hardware.HardwareMonad

data CPU s = CPU
  { registers      :: MVector s Word8
  , stackPointer   :: STRef s Word16
  , programCounter :: STRef s Word16
  , ime :: STRef s Bool
  }

newCPU :: ST s (CPU s)
newCPU = CPU
    <$> V.replicate 0x8 0x00
    <*> newSTRef 0x0000
    <*> newSTRef 0x0000
    <*> newSTRef False

data GBState s = GBState
  { hiram :: MVector s Word8
  , ram   :: MVector s Word8

  , cpu :: CPU s

  , shouldStop   :: STRef s Bool

  , gbTimer     :: STRef s Timer
  , gbInterrupt :: STRef s InterruptState
  , gbGPU       :: GPUState s
  , gbJoypad    :: STRef s JoypadState

  , gbCartridge   :: CartridgeState s
  }

newtype GBT s m a = GBT (ReaderT (GBState s) m a)
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (GBT s m) where
  liftIO f = GBT $ liftIO f

type GB = GBT RealWorld

makeGBState :: CartridgeState s -> ST s (GBState s)
makeGBState cart = do
  hiram <- V.replicate 0x7f 0x00
  ram <- V.replicate 0x2000 0x00
  GBState
    <$> pure hiram
    <*> pure ram
    <*> newCPU
    <*> newSTRef False
    <*> newSTRef defaultTimer
    <*> newSTRef defaultInterruptState
    <*> defaultGPUState
    <*> newSTRef defaultJoypadState
    <*> pure cart

runGB :: MonadIO m => CartridgeState RealWorld -> GB m a -> m a
runGB cart (GBT x) = do
  gbState <- liftIO $ stToIO $ makeGBState cart
  runReaderT x gbState

runGBnoBoot cart f = runGB cart $ do
  storeAddr 0xff50 0x01 -- disable boot rom
  storePC 0x100
  f

readState :: MonadIO m => (GBState s -> STRef RealWorld a) -> GBT s m a
readState  f   = GBT $ liftIO . stToIO . readSTRef =<< asks f

writeState :: MonadIO m => (GBState s -> STRef RealWorld a) -> a -> GBT s m ()
writeState f b = GBT $ liftIO . stToIO . (`writeSTRef` b) =<< asks f

modifyState :: MonadIO m
            => (GBState s -> STRef RealWorld a)
            -> (a -> a)
            -> GBT s m ()
modifyState accessor f = writeState accessor . f =<< readState accessor

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

  readGPURAM addr = GBT $ liftIO . stToIO . loadGPURAM addr =<< asks gbGPU
  writeGPURAM addr byte' = GBT $ liftIO . stToIO . storeGPURAM addr byte' =<< asks gbGPU

  readOAM addr = GBT $ liftIO . stToIO . loadGPUOAM addr =<< asks gbGPU
  writeOAM addr byte' = GBT $ liftIO . stToIO . storeGPUOAM addr byte' =<< asks gbGPU

  getGPUControl = readState (gpuConfig . gbGPU)
  putGPUControl = writeState (gpuConfig . gbGPU)

  fillOAMUnsafe v = GBT $ liftIO . stToIO . fillGPUOAMUnsafe v =<< asks gbGPU

  updateGPUInternal cycles = GBT $ liftIO . stToIO . updateGPUState cycles =<< asks gbGPU

  getInterrupt = readState  gbInterrupt
  putInterrupt = writeState gbInterrupt

  getJoypad = readState gbJoypad
  putJoypad = writeState gbJoypad

  readCartridge addr = GBT $ liftIO . stToIO . loadCartridge addr =<< asks gbCartridge

  writeCartridge addr byte' = GBT $ liftIO . stToIO . storeCartridge addr byte' =<< asks gbCartridge

  readCartridgeRAM addr = GBT $ liftIO . stToIO . loadCartridgeRAM addr =<< asks gbCartridge

  writeCartridgeRAM addr byte' = GBT $ liftIO . stToIO . storeCartridgeRAM addr byte' =<< asks gbCartridge

  disableBootRom = GBT $ liftIO . stToIO . storeCartridgeBootRomRegister =<< asks gbCartridge

  bootRomDisabled = GBT $ liftIO . stToIO . loadCartridgeBootRomRegister =<< asks gbCartridge

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
    regs <- asks (registers . cpu)
    liftIO $ V.write regs (reg8index r) b

  storeSP = writeState (stackPointer . cpu)
  storePC = writeState (programCounter . cpu)

  loadReg r = GBT $ do
    regs <- asks (registers . cpu)
    liftIO $ V.read regs (reg8index r)

  loadSP = readState (stackPointer . cpu)
  loadPC = readState (programCounter . cpu)

  setStop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ writeSTRef s True

  stop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ readSTRef s

  getIME = readState  (ime . cpu)
  setIME = writeState (ime . cpu)

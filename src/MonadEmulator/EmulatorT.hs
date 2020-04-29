{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances, TypeFamilies #-}
module MonadEmulator.EmulatorT
( MonadEmulator(..)
, EmulatorT
, runEmulatorT
, saveEmulatorT

, Emulator
, runEmulator
)
where

import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed.Mutable (MVector)
import Data.STRef
import Data.Word

import Control.Lens
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Reader

import MonadEmulator.Class
import Hardware.HardwareMonad
import CPU.Registers

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

data EmulatorState s = EmulatorState
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

newtype EmulatorT m a = EmulatorT (ReaderT (EmulatorState (PrimState m)) m a)
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (EmulatorT m) where
  liftIO f = EmulatorT $ liftIO f

type Emulator = EmulatorT IO

makeGBState :: CartridgeState s -> ST s (EmulatorState s)
makeGBState cart = do
  hiram <- V.replicate 0x7f 0x00
  ram <- V.replicate 0x2000 0x00
  EmulatorState
    <$> pure hiram
    <*> pure ram
    <*> newCPU
    <*> newSTRef False
    <*> newSTRef defaultTimer
    <*> newSTRef defaultInterruptState
    <*> defaultGPUState
    <*> newSTRef defaultJoypadState
    <*> pure cart

runEmulatorT :: PrimMonad m => Maybe BootRom -> Rom -> EmulatorT m a -> m a
runEmulatorT brom rom (EmulatorT x) = do
  cart <- stToPrim $ makeCartridge brom rom
  gbState <- stToPrim $ makeGBState cart
  runReaderT x gbState

runEmulator :: Maybe BootRom -> Rom -> Emulator a -> IO a
runEmulator = runEmulatorT

runEmulatorTnoBoot :: PrimMonad m => Rom -> EmulatorT m a -> m a
runEmulatorTnoBoot rom f = runEmulatorT Nothing rom $ do
  disableBootRom
  storePC 0x100
  f

saveEmulatorT :: PrimMonad m => EmulatorT m (Maybe CartridgeRAMSave)
saveEmulatorT = EmulatorT $ do
  cart <- asks gbCartridge
  lift $ stToPrim $ saveCartridge cart

readState :: (PrimMonad m, s ~ PrimState m) => (EmulatorState s -> STRef s a) -> EmulatorT m a
readState f = EmulatorT $ stToPrim . readSTRef =<< asks f

writeState :: (PrimMonad m, s ~ PrimState m) => (EmulatorState s -> STRef s a) -> a -> EmulatorT m ()
writeState f b = EmulatorT $ stToPrim . (`writeSTRef` b) =<< asks f

modifyState :: (PrimMonad m, s ~ PrimState m)
            => (EmulatorState s -> STRef s a)
            -> (a -> a)
            -> EmulatorT m ()
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

instance PrimMonad m => HardwareMonad (EmulatorT m) where

  getTimer = readState  gbTimer
  putTimer = writeState gbTimer

  readGPURAM addr = EmulatorT $ stToPrim . loadGPURAM addr =<< asks gbGPU
  writeGPURAM addr byte' = EmulatorT $ stToPrim . storeGPURAM addr byte' =<< asks gbGPU

  readOAM addr = EmulatorT $ stToPrim . loadGPUOAM addr =<< asks gbGPU
  writeOAM addr byte' = EmulatorT $ stToPrim . storeGPUOAM addr byte' =<< asks gbGPU

  getGPUControl = readState (gpuConfig . gbGPU)
  putGPUControl = writeState (gpuConfig . gbGPU)

  fillOAMUnsafe v = EmulatorT $ stToPrim . fillGPUOAMUnsafe v =<< asks gbGPU

  updateGPUInternal cycles = EmulatorT $ stToPrim . updateGPUState cycles =<< asks gbGPU

  getInterrupt = readState  gbInterrupt
  putInterrupt = writeState gbInterrupt

  getJoypad = readState gbJoypad
  putJoypad = writeState gbJoypad

  readCartridge addr = EmulatorT $ stToPrim . loadCartridge addr =<< asks gbCartridge

  writeCartridge addr byte' = EmulatorT $ stToPrim . storeCartridge addr byte' =<< asks gbCartridge

  readCartridgeRAM addr = EmulatorT $ stToPrim . loadCartridgeRAM addr =<< asks gbCartridge

  writeCartridgeRAM addr byte' = EmulatorT $ stToPrim . storeCartridgeRAM addr byte' =<< asks gbCartridge

  disableBootRom = EmulatorT $ stToPrim . storeCartridgeBootRomRegister =<< asks gbCartridge

  bootRomDisabled = EmulatorT $ stToPrim . loadCartridgeBootRomRegister =<< asks gbCartridge

  readRAM idx = EmulatorT $ do
    ram <- asks ram
    stToPrim $ V.read ram (fromIntegral idx)

  readHRAM idx = EmulatorT $ do
    hiram <- asks hiram
    stToPrim $ V.read hiram (fromIntegral idx)

  writeRAM idx b = EmulatorT $ do
    ram <- asks ram
    stToPrim $ V.write ram (fromIntegral idx) b

  writeHRAM idx b = EmulatorT $ do
    hiram <- asks hiram
    stToPrim $ V.write hiram (fromIntegral idx) b

instance PrimMonad m => MonadEmulator (EmulatorT m) where

  storeReg r b = EmulatorT $ do
    regs <- asks (registers . cpu)
    stToPrim $ V.write regs (reg8index r) b

  storeSP = writeState (stackPointer . cpu)
  storePC = writeState (programCounter . cpu)

  loadReg r = EmulatorT $ do
    regs <- asks (registers . cpu)
    stToPrim $ V.read regs (reg8index r)

  loadSP = readState (stackPointer . cpu)
  loadPC = readState (programCounter . cpu)

  setStop = EmulatorT $ do
    s <- asks shouldStop
    stToPrim $ writeSTRef s True

  stop = EmulatorT $ do
    s <- asks shouldStop
    stToPrim $ readSTRef s

  getIME = readState  (ime . cpu)
  setIME = writeState (ime . cpu)

  storeAddr = storeMem
  loadAddr = loadMem

  {-# INLINE anyInterrupts #-}
  anyInterrupts = checkForInterrupts <$> getInterrupt
  {-# INLINE clearInterrupt #-}
  clearInterrupt i = modifyInterrupt (interruptFlag i .~ False)

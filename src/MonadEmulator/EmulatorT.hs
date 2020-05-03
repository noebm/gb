{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module MonadEmulator.EmulatorT
  ( MonadEmulator(..)
  , EmulatorConfig(..)

  , EmulatorT
  , runEmulatorT
  , saveEmulatorT

  , Emulator
  , runEmulator
  , saveEmulator
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

data EmulatorState m s = EmulatorState
  { emuHiram :: MVector s Word8
  , emuRam   :: MVector s Word8

  , emuCpu   :: CPU s

  , emuShouldStop :: STRef s Bool

  , emuTimer     :: STRef s Timer
  , emuInterrupt :: STRef s InterruptState
  , emuGPU       :: GPUState s
  , emuJoypad    :: STRef s JoypadState
  , emuSerialPort :: STRef s SerialPort
  , emuSerialEndpoint :: Word8 -> m (Maybe Word8)

  , emuCartridge :: CartridgeState s
  }

newtype EmulatorT m a = EmulatorT (ReaderT (EmulatorState m (PrimState m)) m a)
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (EmulatorT m) where
  liftIO f = EmulatorT $ liftIO f

type Emulator = EmulatorT IO

makeEmulatorState :: EmulatorConfig m -> ST s (EmulatorState m s)
makeEmulatorState config = EmulatorState
  <$> V.replicate 0x7f 0x00
  <*> V.replicate 0x2000 0x00
  <*> newCPU
  <*> newSTRef False
  <*> newSTRef defaultTimer
  <*> newSTRef defaultInterruptState
  <*> defaultGPUState
  <*> newSTRef defaultJoypadState
  <*> newSTRef defaultSerialPort
  <*> pure (emulatorSerialEndpoint config)
  <*> makeCartridge (emulatorUseBootRom config) (emulatorRom config)

data EmulatorConfig m = EmulatorConfig
  { emulatorUseBootRom :: Maybe BootRom
  , emulatorRom :: Rom
  , emulatorSerialEndpoint :: Word8 -> m (Maybe Word8)
  }

runEmulatorT :: PrimMonad m => EmulatorConfig m -> EmulatorT m a -> m a
runEmulatorT config (EmulatorT m) = stToPrim (makeEmulatorState config) >>= runReaderT m

runEmulator :: EmulatorConfig IO -> Emulator a -> IO a
runEmulator = runEmulatorT

saveEmulatorT :: PrimMonad m => EmulatorT m (Maybe CartridgeRAMSave)
saveEmulatorT = helper saveCartridge emuCartridge

saveEmulator :: Emulator (Maybe CartridgeRAMSave)
saveEmulator = saveEmulatorT

readState :: (PrimMonad m, s ~ PrimState m)
          => (EmulatorState m s -> STRef s a) -> EmulatorT m a
readState = helper readSTRef

writeState :: (PrimMonad m, s ~ PrimState m)
           => (EmulatorState m s -> STRef s a) -> a -> EmulatorT m ()
writeState f b = helper (`writeSTRef` b) f

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

{-# INLINE helper #-}
helper :: PrimMonad m
         => (s -> ST (PrimState m) b) -> (EmulatorState m (PrimState m) -> s) -> EmulatorT m b
helper f get = EmulatorT $ stToPrim . f =<< asks get

instance PrimMonad m => HardwareMonad (EmulatorT m) where

  getTimer = readState  emuTimer
  putTimer = writeState emuTimer

  readGPURAM addr = helper (loadGPURAM addr) emuGPU
  writeGPURAM addr byte' = helper (storeGPURAM addr byte') emuGPU

  readOAM addr = helper (loadGPUOAM addr) emuGPU
  writeOAM addr byte' = helper (storeGPUOAM addr byte') emuGPU

  getGPUControl = readState (gpuConfig . emuGPU)
  putGPUControl = writeState (gpuConfig . emuGPU)

  fillOAMUnsafe v = helper (fillGPUOAMUnsafe v) emuGPU

  updateGPUInternal cycles = helper (updateGPUState cycles) emuGPU

  getInterrupt = readState  emuInterrupt
  putInterrupt = writeState emuInterrupt

  getJoypad = readState emuJoypad
  putJoypad = writeState emuJoypad

  readCartridge addr = helper (loadCartridge addr) emuCartridge
  writeCartridge addr byte' = helper (storeCartridge addr byte') emuCartridge

  readCartridgeRAM addr = helper (loadCartridgeRAM addr) emuCartridge
  writeCartridgeRAM addr byte' = helper (storeCartridgeRAM addr byte') emuCartridge

  disableBootRom = helper storeCartridgeBootRomRegister emuCartridge
  bootRomDisabled = helper loadCartridgeBootRomRegister emuCartridge

  readRAM  idx   = helper (`V.read` fromIntegral idx) emuRam
  writeRAM idx b = helper (\mem -> V.write mem (fromIntegral idx) b) emuRam

  readHRAM  idx   = helper (`V.read` fromIntegral idx) emuHiram
  writeHRAM idx b = helper (\mem -> V.write mem (fromIntegral idx) b) emuHiram

  getSerialPort        = helper readSTRef emuSerialPort
  putSerialPort serial = helper (`writeSTRef` serial) emuSerialPort

  {-# INLINE serialEndpoint #-}
  serialEndpoint b = EmulatorT $ lift . ($ b) =<< asks emuSerialEndpoint

instance PrimMonad m => MonadEmulator (EmulatorT m) where

  storeReg r b = helper (\regs -> V.write regs (reg8index r) b) (registers . emuCpu)
  loadReg  r   = helper (\regs -> V.read regs (reg8index r)) (registers . emuCpu)

  storeSP = writeState (stackPointer . emuCpu)
  storePC = writeState (programCounter . emuCpu)

  loadSP = readState (stackPointer . emuCpu)
  loadPC = readState (programCounter . emuCpu)

  setStop = helper (`writeSTRef` True) emuShouldStop
  stop    = helper readSTRef emuShouldStop

  getIME = readState  (ime . emuCpu)
  setIME = writeState (ime . emuCpu)

  storeAddr = storeMem
  loadAddr = loadMem

  {-# INLINE anyInterrupts #-}
  anyInterrupts = checkForInterrupts <$> getInterrupt
  {-# INLINE clearInterrupt #-}
  clearInterrupt i = modifyInterrupt (interruptFlag i .~ False)

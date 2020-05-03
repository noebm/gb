module Hardware.HardwareMonad
  ( module Hardware.Timer

  , module Hardware.GPU
  , module Hardware.Joypad

  , module Hardware.Cartridge
  , module Hardware.BootRom
  , module Hardware.Serial

  , module Hardware.Interrupt

  , HardwareMonad (..)

  , loadMem, storeMem

  , modifyInterrupt

  , tickHardware
  , setJoypad

  , word16
  )
where

import Control.Lens
import Control.Monad

import Data.Word
import qualified Data.Vector.Unboxed as V

import Hardware.Interrupt

import Hardware.Timer
import Hardware.Joypad
import Hardware.GPU
import Hardware.Cartridge
import Hardware.BootRom
import Hardware.Serial

import Data.Bits

class Monad m => HardwareMonad m where
  readGPURAM :: Word16 -> m Word8
  writeGPURAM :: Word16 -> Word8 -> m ()

  readOAM :: Word16 -> m Word8
  writeOAM :: Word16 -> Word8 -> m ()

  getGPUControl :: m GPUControl
  putGPUControl :: GPUControl -> m ()

  fillOAMUnsafe :: V.Vector Word8 -> m ()

  updateGPUInternal :: Word -> m (Bool, Maybe Frame)

  getInterrupt :: m InterruptState
  putInterrupt :: InterruptState -> m ()

  getTimer :: m Timer
  putTimer :: Timer -> m ()

  getJoypad :: m JoypadState
  putJoypad :: JoypadState -> m ()

  -- 0x0000 - 0x7fff
  readCartridge :: Word16 -> m Word8
  writeCartridge :: Word16 -> Word8 -> m ()

  -- 0xa000 - 0xbfff
  readCartridgeRAM :: Word16 -> m Word8
  writeCartridgeRAM :: Word16 -> Word8 -> m ()

  -- 0xff50
  disableBootRom :: m ()
  bootRomDisabled :: m Bool

  -- 0xC000 - 0xDFFF
  readRAM :: Word16 -> m Word8
  writeRAM :: Word16 -> Word8 -> m ()

  -- 0xFF80 - 0xFFFE
  readHRAM :: Word16 -> m Word8
  writeHRAM :: Word16 -> Word8 -> m ()

  getSerialPort :: m SerialPort
  putSerialPort :: SerialPort -> m ()
  -- serialEndpoint :: Word8 -> m (Maybe Word8)

modifyInterrupt :: HardwareMonad m => (InterruptState -> InterruptState) -> m ()
modifyInterrupt f = putInterrupt . f =<< getInterrupt

{-# INLINE tickHardware #-}
tickHardware :: HardwareMonad m => Maybe (SerialConnection m) -> Word -> m (Maybe Frame)
tickHardware f cyc = do
  updateTimer' cyc
  updateSerialPort f cyc
  updateGPU' cyc

updateGPU' :: HardwareMonad m => Word -> m (Maybe Frame)
updateGPU' cyc = do
  (flag, im) <- updateGPUInternal cyc
  forM_ im $ \_ -> modifyInterrupt $ interruptFlag INTVBLANK .~ True
  when flag $      modifyInterrupt $ interruptFlag INTLCD .~ True
  return im

updateTimer' :: HardwareMonad m => Word -> m ()
updateTimer' cycles = do
  ts <- getTimer
  let (overflow, ts') = updateTimer cycles ts
  putTimer ts'
  when overflow $ modifyInterrupt $ interruptFlag INTTIMER .~ True

{-# INLINE updateSerialPort #-}
updateSerialPort :: HardwareMonad m => Maybe (SerialConnection m) -> Word -> m ()
updateSerialPort f cycles = do
  serial <- getSerialPort
  (reqInt, serial') <- tickSerial f cycles serial
  putSerialPort serial'
  when reqInt $ modifyInterrupt $ interruptFlag INTSERIAL .~ True

setJoypad :: HardwareMonad m => (Joypad , Bool) -> m ()
setJoypad f = do
  putJoypad . updateJoypad f =<< getJoypad
  when (snd f) $ modifyInterrupt $ interruptFlag INTJOYPAD .~ True

{-# INLINE word16 #-}
word16 :: Iso' (Word8, Word8) Word16
word16 = iso
  (\(h,l) -> shift (fromIntegral h) 8 .|. fromIntegral l)
  (\w -> (fromIntegral $ shift (w .&. 0xff00) (negate 8), fromIntegral $ w .&. 0x00ff))

-- currently not implemented
inSoundRange :: Word16 -> Bool
inSoundRange addr = 0xFF10 <= addr && addr < 0xFF40

-- currently not implemented
inSerialRange :: Word16 -> Bool
inSerialRange addr = 0xFF01 <= addr && addr < 0xFF03

{-# INLINE storeMem #-}
storeMem :: HardwareMonad m
         => Word16 -> Word8 -> m ()
storeMem idx b
  | idx < 0x8000 = writeCartridge idx b
  | idx < 0xA000 = writeGPURAM idx b
  | idx < 0xC000 = writeCartridgeRAM idx b
  | idx < 0xFE00 = writeRAM (idx .&. 0x1FFF) b -- ram + echo ram
  | idx < 0xFEA0 = writeOAM idx b
  | idx < 0xFF00 = return ()

  | idx == 0xff00 = putJoypad . storeJoypad b =<< getJoypad
  | idx == 0xff01 = putSerialPort . storeSerial idx b =<< getSerialPort
  | idx == 0xff02 = putSerialPort . storeSerial idx b =<< getSerialPort
  | idx == 0xff0f = putInterrupt =<< storeIntFlag b <$> getInterrupt
  | idx == 0xffff = putInterrupt =<< storeIntEnable b <$> getInterrupt
  | inTimerRange idx = putTimer . storeTimer idx b =<< getTimer
  | idx == 0xff46 = do
      let baseaddr = (b , 0x00) ^. word16
      vec <- V.generateM 0xa0 $ loadMem . fromIntegral . (fromIntegral baseaddr +)
      fillOAMUnsafe vec
  | 0xFF40 <= idx && idx < 0xFF50 = putGPUControl . storeGPUControl idx b =<< getGPUControl
  | idx == 0xff50 = when (b `testBit` 0) disableBootRom
  -- unimplemented IO port
  | 0xFF00 <= idx && idx < 0xFF80 = return ()

  | idx < 0xFFFF = writeHRAM (idx - 0xFF80) b
  | otherwise = error $ "storeMem: writing " ++ show b ++ " to " ++ show idx

{-# INLINE loadMem #-}
loadMem :: HardwareMonad m => Word16 -> m Word8
loadMem idx
  | idx < 0x8000 = readCartridge idx
  | idx < 0xA000 = readGPURAM idx
  | idx < 0xC000 = readCartridgeRAM idx
  | idx < 0xFE00 = readRAM (idx .&. 0x1FFF) -- ram + echo ram
  | idx < 0xFEA0 = readOAM idx
  | idx < 0xFF00 = return 0x00

  | idx == 0xff00 = loadJoypad <$> getJoypad
  | idx == 0xff01 = loadSerial idx <$> getSerialPort
  | idx == 0xff02 = loadSerial idx <$> getSerialPort
  | 0xFF40 <= idx && idx < 0xFF50 = loadGPUControl idx <$> getGPUControl
  | idx == 0xff50 = (\x -> if x then 0xff else 0xfe) <$> bootRomDisabled

  | idx == 0xff0f = loadIntFlag <$> getInterrupt
  | idx == 0xffff = loadIntEnable <$> getInterrupt
  | inTimerRange idx = do
      ts <- getTimer
      return $ loadTimer ts idx
  -- unimplemented IO port
  | 0xFF00 <= idx && idx < 0xFF80 = return 0x00
  | idx < 0xFFFF = readHRAM (idx - 0xFF80)
  | otherwise = error $ "loadMem: access to " ++ show idx

module HardwareMonad where

import Control.Lens
import Control.Monad

import Data.Word

import Timer
import Interrupt.Interrupt
import Interrupt.InterruptType
import GPU.GPUState
import Joypad as Joypad
import Cartridge.Cartridge

import Control.Lens
import Data.Bits

import Debug.Trace

class Monad m => HardwareMonad m where
  getGPU :: m GPUState
  putGPU :: GPUState -> m ()

  getInterrupt :: m InterruptState
  putInterrupt :: InterruptState -> m ()

  getTimerState :: m TimerState
  putTimerState :: TimerState -> m ()

  getJoypad :: m JoypadState
  putJoypad :: JoypadState -> m ()

  getCartridge :: m CartridgeState
  putCartridge :: CartridgeState -> m ()

  -- 0xC000 - 0xDFFF
  readRAM :: Word16 -> m Word8
  writeRAM :: Word16 -> Word8 -> m ()

  -- 0xFF80 - 0xFFFE
  readHRAM :: Word16 -> m Word8
  writeHRAM :: Word16 -> Word8 -> m ()

modifyInterrupt f = putInterrupt . f =<< getInterrupt

updateGPU :: HardwareMonad m => Word -> (GPUState -> GPURequest -> m ()) -> m ()
updateGPU cyc f = do
  (flag, req,  gpu') <- updateGPUState cyc <$> getGPU
  putGPU gpu'
  forM_ req $ \req -> do
    when (req == Draw) $ modifyInterrupt $ interruptVBlank.interruptFlag .~ True
    f gpu' req
  when flag $ modifyInterrupt $ interruptLCD.interruptFlag .~ True

updateTimer :: HardwareMonad m => Word -> m ()
updateTimer cycles = do
  ts <- getTimerState
  let (overflow, ts') = updateTimerState cycles ts
  putTimerState ts'
  when overflow $
    modifyInterrupt $ interruptTimer.interruptFlag .~ True

updateJoypad :: HardwareMonad m => (Joypad , Bool) -> m ()
updateJoypad f = do
  s0 <- getJoypad
  let s1 = Joypad.updateJoypad f s0
  putJoypad s1
  when (snd f) $ modifyInterrupt $ interruptJoypad.interruptFlag .~ True

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
  | idx < 0x8000 = putCartridge . storeCartridge idx b =<< getCartridge
  | idx < 0xA000 = putGPU . storeGPURAM idx b =<< getGPU
  | idx < 0xC000 = putCartridge . storeCartridgeRAM idx b =<< getCartridge
  | idx < 0xFE00 = writeRAM (idx .&. 0x1FFF) b -- ram + echo ram
  | idx < 0xFEA0 = putGPU . storeGPUOAM idx b =<< getGPU
  | idx < 0xFF00 = return ()

  | inJoypadRange idx = do
      s <- getJoypad
      putJoypad $ storeJoypad idx b s
  | inInterruptRange idx = do
      s <- getInterrupt
      putInterrupt $ storeInterrupt s idx b
  | inTimerRange idx = do
      ts <- getTimerState
      let ts' = storeTimer idx b ts
      putTimerState ts'
  | idx == 0xff46 = do
      gpu <- getGPU
      gpu' <- dmaTransfer loadMem ((b , 0x00) ^. word16) gpu
      putGPU gpu'
  | 0xFF40 <= idx && idx < 0xFF50 = putGPU . storeGPURegisters idx b =<< getGPU
  | idx == 0xff50 = putCartridge . storeCartridgeBootRomRegister b =<< getCartridge
  -- unimplemented IO port
  | 0xFF00 <= idx && idx < 0xFF80 = return ()

  | idx < 0xFFFF = writeHRAM (idx - 0xFF80) b
  | otherwise = error $ "storeMem: writing " ++ show b ++ " to " ++ show idx

{-# INLINE loadMem #-}
loadMem :: HardwareMonad m => Word16 -> m Word8
loadMem idx
  | idx < 0x8000 = loadCartridge idx <$> getCartridge
  | idx < 0xA000 = loadGPURAM idx <$> getGPU
  | idx < 0xC000 = loadCartridgeRAM idx <$> getCartridge
  | idx < 0xFE00 = readRAM (idx .&. 0x1FFF) -- ram + echo ram
  | idx < 0xFEA0 = loadGPUOAM idx <$> getGPU
  | idx < 0xFF00 = return 0x00

  | inJoypadRange idx = do
      s <- getJoypad
      return $ loadJoypad s idx
  | 0xFF40 <= idx && idx < 0xFF50 = loadGPURegisters idx <$> getGPU
  | idx == 0xff50 = loadCartridgeBootRomRegister <$> getCartridge

  | inInterruptRange idx = do
      s <- getInterrupt
      return $ loadInterrupt s idx
  | inTimerRange idx = do
      ts <- getTimerState
      return $ loadTimer ts idx
  -- unimplemented IO port
  | 0xFF00 <= idx && idx < 0xFF80 = return 0x00
  | idx < 0xFFFF = readHRAM (idx - 0xFF80)
  | otherwise = error $ "loadMem: access to " ++ show idx

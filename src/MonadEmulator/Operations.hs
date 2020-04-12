module MonadEmulator.Operations
  ( MonadEmulator (..)
  , storeReg16, loadReg16
  , storeAddr16, loadAddr16

  , Interrupt
  , serviceInterrupt

  , flagC, flagH, flagN, flagZ
  , byte, word, sbyte

  , addRelative
  , jump, jumpRelative
  , push, pop
  , call, ret
  , restart

  , word16

  , module CPU.Registers
  )
where

import Control.Lens
import Data.Bits.Lens

import Data.Word
import Data.Int

import MonadEmulator.Class
import Hardware.Interrupt
import Hardware.HardwareMonad (word16)
import CPU.Registers

{-# INLINE load16LE #-}
load16LE :: Monad m => m Word8 -> m Word8 -> m Word16
load16LE b0 b1 = flip (curry (view word16)) <$> b0 <*> b1

{-# INLINE store16LE #-}
store16LE :: Monad m => (Word8 -> m ()) -> (Word8 -> m ()) -> (Word16 -> m ())
store16LE b0 b1 w = let (h , l) = w ^. from word16 in b0 l >> b1 h

storeAddr16 :: MonadEmulator m => Word16 -> Word16 -> m ()
storeAddr16 addr = store16LE (storeAddr addr) (storeAddr $ addr + 1)

storeReg16 :: MonadEmulator m => Reg16 -> Word16 -> m ()
storeReg16 r =
  let (r0, r1) = regPair r
  in store16LE (storeReg r1) (storeReg r0)

loadAddr16 :: MonadEmulator m => Word16 -> m Word16
loadAddr16 addr = load16LE (loadAddr addr) (loadAddr $ addr + 1)

loadReg16 :: MonadEmulator m => Reg16 -> m Word16
loadReg16 r =
  let (r0, r1) = regPair r
  in load16LE (loadReg r1) (loadReg r0)

serviceInterrupt :: (MonadEmulator m) => Interrupt -> m ()
serviceInterrupt i = do
  clearInterrupt i
  setIME False
  call (interruptAddress i)

{-# INLINE flagZ #-}
{-# INLINE flagN #-}
{-# INLINE flagH #-}
{-# INLINE flagC #-}
flagZ, flagN, flagH, flagC :: Lens' Word8 Bool
flagZ = bitAt 7
flagN = bitAt 6
flagH = bitAt 5
flagC = bitAt 4

{-# INLINE byte #-}
byte :: MonadEmulator m => m Word8
byte = do
  pc <- loadPC
  storePC (pc + 1)
  loadAddr pc

{-# INLINE word #-}
word :: MonadEmulator m => m Word16
word = load16LE byte byte

{-# INLINE sbyte #-}
sbyte :: MonadEmulator m => m Int8
sbyte = fromIntegral <$> byte

jump :: MonadEmulator m => Word16 -> m ()
jump = storePC

{-# INLINE addRelative #-}
addRelative :: Word16 -> Int8 -> Word16
addRelative addr x = fromIntegral $ (fromIntegral addr :: Int) + fromIntegral x

jumpRelative :: MonadEmulator m => Int8 -> m ()
jumpRelative addrdiff = do
  pc <- loadPC
  jump $ addRelative pc addrdiff

push :: MonadEmulator m => Word16 -> m ()
push w = do
  sp <- loadSP
  storeAddr16 (sp - 2) w
  storeSP (sp - 2)

pop :: MonadEmulator m => m Word16
pop = do
  sp <- loadSP
  storeSP (sp + 2)
  loadAddr16 sp

call :: MonadEmulator m => Word16 -> m ()
call addr = do
  push =<< loadPC
  jump addr

ret :: MonadEmulator m => m ()
ret = jump =<< pop

restart :: MonadEmulator m => Word8 -> m ()
restart b = do
  push =<< loadPC
  jump $ (0x00, b) ^. word16

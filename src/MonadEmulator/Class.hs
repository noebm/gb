{-# LANGUAGE DefaultSignatures #-}
module MonadEmulator.Class where

import Control.Lens

import Hardware.HardwareMonad
import CPU.Registers
import Data.Word

-- | Allow reading and writing inside the address space.
-- All loads and stores should be idempotent.
-- load-store should be the identity for registers.
-- store-load should be equal to store.
class Monad m => MonadEmulator m where

  storeAddr :: Word16 -> Word8 -> m ()
  loadAddr  :: Word16 -> m Word8

  default storeAddr   :: HardwareMonad m => Word16 -> Word8 -> m ()
  storeAddr = storeMem

  default loadAddr :: HardwareMonad m => Word16 -> m Word8
  loadAddr = loadMem

  storeReg :: Reg8 -> Word8 -> m ()
  loadReg :: Reg8 -> m Word8

  storePC :: Word16 -> m ()
  loadPC :: m Word16

  storeSP :: Word16 -> m ()
  loadSP :: m Word16

  setStop :: m ()
  stop :: m Bool

  getIME :: m Bool
  setIME :: Bool -> m ()

  anyInterrupts :: m (Maybe Interrupt)
  {-# INLINE anyInterrupts #-}
  default anyInterrupts :: (HardwareMonad m) => m (Maybe Interrupt)
  anyInterrupts = checkForInterrupts <$> getInterrupt

  clearInterrupt :: Interrupt -> m ()
  {-# INLINE clearInterrupt #-}
  default clearInterrupt :: (HardwareMonad m) => Interrupt -> m ()
  clearInterrupt i = modifyInterrupt (interruptFlag i .~ False)

{-# LANGUAGE DefaultSignatures #-}
module MonadEmulator
  ( Reg8 (..)
  , Reg16 (..)
  , MonadEmulator (..)
  , HardwareMonad (..)
  , storeReg16
  , storeAddr16
  , loadReg16
  , loadAddr16

  , updateGPU
  , updateTimer

  , StepInfo (..)
  , prefetch
  , anyInterrupts, serviceInterrupt
  , showRegisters

  , word16

  , flagC, flagH, flagN, flagZ

  , byte, word, sbyte

  , modifyInterrupt

  , addRelative
  , jump, jumpRelative
  , push, pop
  , call, ret
  , restart
  )
where

import Control.Lens
import Control.Monad.State

import Text.Printf

import Data.Bits.Lens
import Data.Word
import Data.Int

import CPU.Registers
import Interrupt.Interrupt
import Interrupt.InterruptType

import HardwareMonad

showRegisters :: MonadEmulator m => m String
showRegisters = do
  let rs8 = [A, F, B, C, D, E, H, L]
  s1 <- forM rs8 $ \r -> do
    v <- loadReg r
    return $ show r ++ printf ": %02x " v
  s2 <- forM [("PC", loadPC), ("SP", loadSP)] $ \(name, get) -> do
    v <- get
    return $ name ++ printf ": %04x " v
  return $ concat s1 ++ concat s2

{-# INLINE load16LE #-}
load16LE :: Monad m => m Word8 -> m Word8 -> m Word16
load16LE b0 b1 = do
  l <- b0
  h <- b1
  return $ (h , l) ^. word16

{-# INLINE store16LE #-}
store16LE :: Monad m => (Word8 -> m ()) -> (Word8 -> m ()) -> (Word16 -> m ())
store16LE b0 b1 w = let (h , l) = w ^. from word16 in b0 l >> b1 h

-- | Allow reading and writing inside the address space.
-- All loads and stores should be idempotent.
-- load-store should be the identity for registers.
-- store-load should be equal to store.
class Monad m => MonadEmulator m where
  storeReg :: Reg8 -> Word8 -> m ()
  storeAddr   :: Word16 -> Word8 -> m ()
  default storeAddr   :: HardwareMonad m => Word16 -> Word8 -> m ()
  storeAddr = storeMem

  storePC :: Word16 -> m ()
  storeSP :: Word16 -> m ()

  loadReg :: Reg8 -> m Word8

  loadAddr :: Word16 -> m Word8
  default loadAddr :: HardwareMonad m => Word16 -> m Word8
  loadAddr = loadMem

  loadPC :: m Word16
  loadSP :: m Word16

  setStop :: m ()
  stop :: m Bool

  getIME :: m Bool
  setIME :: Bool -> m ()

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

{-# INLINE aux0 #-}
{-# INLINE aux1 #-}
{-# INLINE aux2 #-}
aux0 :: Functor m => m a -> StateT s m a
aux0 x = StateT $ \s -> flip (,) s <$> x
aux1 :: Functor m => (a -> m b) -> (a -> StateT s m b)
aux1 f  = aux0 . f
aux2 :: Functor m => (a -> b -> m c) -> (a -> b -> StateT s m c)
aux2 f = aux1 . f

instance HardwareMonad m => HardwareMonad (StateT s m) where
  getGPU = aux0 getGPU
  putGPU = aux1 putGPU

  getInterrupt = aux0 getInterrupt
  putInterrupt = aux1 putInterrupt

  getTimerState = aux0 getTimerState
  putTimerState = aux1 putTimerState

  getJoypad = aux0 getJoypad
  putJoypad = aux1 putJoypad

  getCartridge = aux0 getCartridge
  putCartridge = aux1 putCartridge

  readRAM = aux1 readRAM
  writeRAM = aux2 writeRAM

  readHRAM = aux1 readHRAM
  writeHRAM = aux2 writeHRAM

instance MonadEmulator m => MonadEmulator (StateT s m) where
  storeReg = aux2 storeReg
  storeAddr = aux2 storeAddr
  storePC = aux1 storePC
  storeSP = aux1 storeSP

  loadReg = aux1 loadReg
  loadAddr = aux1 loadAddr
  loadPC = aux0 loadPC
  loadSP = aux0 loadSP

  setStop = aux0 setStop
  stop    = aux0 stop

  getIME = aux0 getIME
  setIME = aux1 setIME

data StepInfo = PendingInterrupt !Interrupt | Halt | Running {-# UNPACK #-} !Word8
  deriving Eq

prefetch :: (HardwareMonad m, MonadEmulator m) => m StepInfo
prefetch = do
  i <- anyInterrupts
  ime <- getIME
  maybe (Running <$> byte) (return . PendingInterrupt) (guard ime >> i)

anyInterrupts :: (HardwareMonad m, MonadEmulator m) => m (Maybe Interrupt)
anyInterrupts = checkForInterrupts <$> getInterrupt

serviceInterrupt :: (HardwareMonad m, MonadEmulator m) => Interrupt -> m ()
serviceInterrupt i = do
  modifyInterrupt (interrupt i . interruptFlag .~ False)
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

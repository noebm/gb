module MonadEmulator
  ( Reg8 (..)
  , Reg16 (..)
  , LoadStore8 (..)
  , LoadStore16 (..)
  , MonadEmulator (..)
  , load8, store8
  , load16, store16

  , updateGPU
  , updateTimer
  , processInterrupts
  , getIEM , setIEM
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
import Control.Monad
import Control.Monad.State

import Text.Printf

import Data.Bits.Lens
import Data.Bits
import Data.Word
import Data.Int
import Data.Maybe

import CPU.Registers
import GPU.GPUState
import Interrupt.Interrupt
import Timer
import Interrupt.InterruptType

showRegisters :: MonadEmulator m => m String
showRegisters = do
  let rs8 = [A, F, B, C, D, E, H, L]
  s1 <- forM rs8 $ \r -> do
    v <- load8 (Register8 r)
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

data LoadStore8 = Register8 !Reg8 | Addr8 !Word16
  deriving (Eq, Show)

data LoadStore16 = Register16 !Reg16 | Addr16 !Word16
  deriving (Eq, Show)

-- | Allow reading and writing inside the address space.
-- All loads and stores should be idempotent.
-- load-store should be the identity for registers.
-- store-load should be equal to store.
class Monad m => MonadEmulator m where
  storeReg :: Reg8 -> Word8 -> m ()
  storeAddr   :: Word16 -> Word8 -> m ()
  storePC :: Word16 -> m ()
  storeSP :: Word16 -> m ()

  loadReg :: Reg8 -> m Word8
  loadAddr :: Word16 -> m Word8
  loadPC :: m Word16
  loadSP :: m Word16

  -- | Advances internal timer.
  advCycles :: Word -> m ()
  getCycles :: m Word

  setStop :: m ()
  stop :: m Bool

  setHalt :: m ()
  clearHalt :: m ()
  halt :: m Bool

  getGPU :: m GPUState
  putGPU :: GPUState -> m ()

  getInterrupt :: m InterruptState
  putInterrupt :: InterruptState -> m ()

  getTimerState :: m TimerState
  putTimerState :: TimerState -> m ()

store8 :: MonadEmulator m => LoadStore8 -> Word8 -> m ()
store8 (Register8 r) = storeReg r
store8 (Addr8 addr) = storeAddr addr

store16 :: MonadEmulator m => LoadStore16 -> Word16 -> m ()
store16 (Register16 r) =
  let (r0, r1) = regPair r
  in store16LE (storeReg r1) (storeReg r0)
store16 (Addr16 addr) = store16LE (storeAddr addr) (storeAddr $ addr + 1)

load8 :: MonadEmulator m => LoadStore8 -> m Word8
load8 (Register8 r) = loadReg r
load8 (Addr8 addr) = loadAddr addr

load16 :: MonadEmulator m => LoadStore16 -> m Word16
load16 (Register16 r) =
  let (r0, r1) = regPair r
  in load16LE (loadReg r1) (loadReg r0)
load16 (Addr16 addr) = load16LE (loadAddr addr) (loadAddr $ addr + 1)

modifyInterrupt f = putInterrupt . f =<< getInterrupt

getIEM :: MonadEmulator m => m Bool
getIEM = view interruptMasterEnableFlag <$> getInterrupt

setIEM :: MonadEmulator m => Bool -> m ()
setIEM b = modifyInterrupt $ interruptMasterEnableFlag .~ b

updateGPU :: MonadEmulator m => Word -> (GPUState -> GPURequest -> m ()) -> m ()
updateGPU cyc f = do
  (flag, req,  gpu') <- updateGPUState cyc <$> getGPU
  putGPU gpu'
  forM_ req $ \req -> do
    when (req == Draw) $ modifyInterrupt $ interruptVBlank.interruptFlag .~ True
    f gpu' req
  when flag $ modifyInterrupt $ interruptLCD.interruptFlag .~ True

updateTimer :: MonadEmulator m => Word -> m ()
updateTimer cycles = do
  ts <- getTimerState
  let (overflow, ts') = updateTimerState cycles ts
  putTimerState ts'
  when overflow $
    modifyInterrupt $ interruptTimer.interruptFlag .~ True

gpuInterrupts :: MonadEmulator m => GPUState -> m ()
gpuInterrupts gpu = do
  let conf = gpuConfig gpu
  let lcdInterrupts = [ _gpuOAMInterrupt, _gpuHblankInterrupt, _gpuLineCompareInterrupt ]
  when (any ($ conf) lcdInterrupts) $
    modifyInterrupt $ interruptLCD.interruptFlag .~ True
  when (_gpuVblankInterrupt conf) $
    modifyInterrupt $ interruptVBlank.interruptFlag .~ True

{-# INLINE aux0 #-}
{-# INLINE aux1 #-}
{-# INLINE aux2 #-}
aux0 :: Functor m => m a -> StateT s m a
aux0 x = StateT $ \s -> flip (,) s <$> x
aux1 :: Functor m => (a -> m b) -> (a -> StateT s m b)
aux1 f  = aux0 . f
aux2 :: Functor m => (a -> b -> m c) -> (a -> b -> StateT s m c)
aux2 f = aux1 . f

instance MonadEmulator m => MonadEmulator (StateT s m) where
  storeReg = aux2 storeReg
  storeAddr = aux2 storeAddr
  storePC = aux1 storePC
  storeSP = aux1 storeSP

  loadReg = aux1 loadReg
  loadAddr = aux1 loadAddr
  loadPC = aux0 loadPC
  loadSP = aux0 loadSP

  advCycles = aux1 advCycles
  getCycles = aux0 getCycles

  getGPU = aux0 getGPU
  putGPU = aux1 putGPU

  getInterrupt = aux0 getInterrupt
  putInterrupt = aux1 putInterrupt

  getTimerState = aux0 getTimerState
  putTimerState = aux1 putTimerState

  setStop = aux0 setStop
  stop    = aux0 stop

  setHalt = aux0 setHalt
  clearHalt = aux0 clearHalt
  halt = aux0 halt

processInterrupts :: MonadEmulator m => m Bool
processInterrupts = do
  int <- checkForInterrupts <$> getInterrupt
  forM_ int $ \i -> do
    s <- getInterrupt
    h <- halt
    when (not h) $ putInterrupt (s & interrupt i . interruptFlag .~ False)
    setIEM False
    call (interruptAddress i)
    advCycles 20
  return $ isJust int

{-# INLINE word16 #-}
word16 :: Iso' (Word8, Word8) Word16
word16 = iso
  (\(h,l) -> shift (fromIntegral h) 8 .|. fromIntegral l)
  (\w -> (fromIntegral $ shift (w .&. 0xff00) (negate 8), fromIntegral $ w .&. 0x00ff))

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
  load8 (Addr8 pc)

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
  store16 (Addr16 (sp - 2)) w
  storeSP (sp - 2)

pop :: MonadEmulator m => m Word16
pop = do
  sp <- loadSP
  storeSP (sp + 2)
  load16 (Addr16 sp)

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

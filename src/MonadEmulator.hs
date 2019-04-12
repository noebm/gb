module MonadEmulator
  ( Reg8 (..)
  , Reg16 (..)
  , LoadStore8 (..)
  , LoadStore16 (..)
  , MonadEmulator (..)

  , updateGPU
  , processInterrupts
  , getIEM , setIEM
  , showRegisters
  , getCycles

  , word16

  , load16LE, store16LE

  , flagC, flagH, flagN, flagZ

  , byte, word, sbyte

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

import GPU.GPUState
import Interrupt.Interrupt

data Reg8 = A | B | C | D | E | F | H | L
  deriving (Eq, Show)

data Reg16 = AF | BC | DE | HL | PC | SP
  deriving (Eq, Show)

showRegisters :: MonadEmulator m => m String
showRegisters = do
  let rs8 = [A, F, B, C, D, E, H, L]
  let rs16 = [PC , SP]
  s1 <- forM rs8 $ \r -> do
    v <- load8 (Register8 r)
    return $ show r ++ printf ": %02x " v
  s2 <- forM rs16 $ \r -> do
    v <- load16 (Register16 r)
    return $ show r ++ printf ": %04x " v
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
  -- | Stores a value in side the emulator state.
  store8 :: LoadStore8 -> Word8 -> m ()
  store16 :: LoadStore16 -> Word16 -> m ()

  -- | Retrieves a value from the emulator state.
  load8 :: LoadStore8 -> m Word8
  load16 :: LoadStore16 -> m Word16

  -- | Advances / Clears internal timer.
  advCycles :: Word -> m ()
  resetCycles :: m Word

  setStop :: m ()
  stop :: m Bool

  getGPU :: m GPUState
  putGPU :: GPUState -> m ()

  getInterrupt :: m InterruptState
  putInterrupt :: InterruptState -> m ()

getIEM :: MonadEmulator m => m Bool
getIEM   = interruptStateEnabled <$> getInterrupt

setIEM :: MonadEmulator m => Bool -> m ()
setIEM b = do
  s <- getInterrupt
  putInterrupt $ s { interruptStateEnabled = b }

updateGPU :: MonadEmulator m => (GPUState -> m a) -> m (Maybe a)
updateGPU f = do
  cyc <- getCycles
  mupdate <- updateGPUState cyc <$> getGPU
  forM mupdate $ \(cyc' , gpu') -> do
    putGPU gpu'
    _ <- resetCycles
    advCycles cyc'
    f gpu'

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
  store8  = aux2 store8
  store16 = aux2 store16

  load8     = aux1 load8
  load16    = aux1 load16

  advCycles = aux1 advCycles
  resetCycles = aux0 resetCycles

  getGPU = aux0 getGPU
  putGPU = aux1 putGPU

  getInterrupt = aux0 getInterrupt
  putInterrupt = aux1 putInterrupt

  setStop = aux0 setStop
  stop    = aux0 stop

getCycles :: MonadEmulator m => m Word
getCycles = do
  t <- resetCycles
  advCycles t
  return t

processInterrupts :: MonadEmulator m => m ()
processInterrupts = do
  int <- handleInterrupt <$> getInterrupt
  forM_ int $ \(i , s) -> do
    putInterrupt s
    call (interruptAddress i)
    advCycles 20

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
  let regPC = Register16 PC
  pc <- load16 regPC
  store16 regPC (pc + 1)
  load8 (Addr8 pc)

{-# INLINE word #-}
word :: MonadEmulator m => m Word16
word = load16LE byte byte

{-# INLINE sbyte #-}
sbyte :: MonadEmulator m => m Int8
sbyte = fromIntegral <$> byte

jump :: MonadEmulator m => Word16 -> m ()
jump = store16 (Register16 PC)

{-# INLINE addRelative #-}
addRelative :: Word16 -> Int8 -> Word16
addRelative addr x = fromIntegral $ (fromIntegral addr :: Int) + fromIntegral x

jumpRelative :: MonadEmulator m => Int8 -> m ()
jumpRelative addrdiff = do
  pc <- load16 (Register16 PC)
  store16 (Register16 PC) $ addRelative pc addrdiff

push :: MonadEmulator m => Word16 -> m ()
push w = do
  sp <- load16 (Register16 SP)
  store16 (Addr16 (sp - 2)) w
  store16 (Register16 SP) (sp - 2)

pop :: MonadEmulator m => m Word16
pop = do
  sp <- load16 (Register16 SP)
  store16 (Register16 SP) (sp + 2)
  load16 (Addr16 sp)

call :: MonadEmulator m => Word16 -> m ()
call addr = do
  push =<< load16 (Register16 PC)
  jump addr

ret :: MonadEmulator m => m ()
ret = jump =<< pop

restart :: MonadEmulator m => Word8 -> m ()
restart b = do
  push =<< load16 (Register16 PC)
  store16 (Register16 PC) $ (0x00, b) ^. word16

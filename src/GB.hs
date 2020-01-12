{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances, TypeFamilies #-}
module GB
( MonadEmulator(..)
, GB
, runGB
, showRegisters

, updateJoypadGB
, getJoypad
, putJoypad
)
where

import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed.Mutable (MVector)
import Data.STRef
import Data.Word
import Text.Printf

import Control.Lens
import Control.Monad.ST
import Control.Monad.Reader

import MonadEmulator

import CPU.Registers
import GPU.GPUState
import Interrupt.Interrupt
import Timer
import Cartridge.Cartridge
import Joypad

data GBState s = GBState
  { addressSpace :: MVector s Word8

  , stackpointer   :: STRef s Word16
  , programCounter :: STRef s Word16

  , clock        :: STRef s Word
  , shouldStop   :: STRef s Bool
  , isHalted     :: STRef s Bool

  , gbTimer     :: STRef s TimerState
  , gbInterrupt :: STRef s InterruptState
  , gbGPU       :: STRef s GPUState
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
  memory <- V.replicate (0x10000 + 0x8) 0x00
  GBState
    <$> pure memory
    <*> newSTRef 0x0000
    <*> newSTRef 0x0000
    <*> newSTRef 0
    <*> newSTRef False
    <*> newSTRef False
    <*> newSTRef defaultTimerState
    <*> newSTRef defaultInterruptState
    <*> newSTRef defaultGPUState
    <*> newSTRef defaultJoypadState
    <*> pure cart

runGB :: MonadIO m => CartridgeState RealWorld -> GB m a -> m a
runGB cart (GBT x) = do
  gbState <- liftIO $ stToIO $ makeGBState cart
  runReaderT x gbState

readState :: MonadIO m => (GBState s -> STRef RealWorld a) -> GBT s m a
readState  f   = GBT $ liftIO . stToIO . readSTRef =<< asks f

writeState :: MonadIO m => (GBState s -> STRef RealWorld a) -> a -> GBT s m ()
writeState f b = GBT $ liftIO . stToIO . (`writeSTRef` b) =<< asks f

getJoypad :: MonadIO m => GB m JoypadState
getJoypad = readState gbJoypad

putJoypad :: MonadIO m => JoypadState -> GB m ()
putJoypad s = writeState gbJoypad s

updateJoypadGB :: MonadIO m => (Joypad -> Bool) -> GB m Bool
updateJoypadGB f = GBT $ do
  ref <- asks gbJoypad
  liftIO . stToIO $ do
    s0 <- readSTRef ref
    let (s1 , changed) = updateJoypad f s0
    writeSTRef ref s1
    return changed

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

{-# INLINE reg8ToIndex #-}
reg8ToIndex :: Reg8 -> Int
reg8ToIndex r = rbase + reg8index r
  where rbase = 0x10000

{-# INLINE echoRam #-}
echoRam :: Int -> Bool
echoRam addr = 0xE000 <= addr && addr < 0xFE00

{-# INLINE loadAddr' #-}
loadAddr' :: MonadIO m => Int -> GB m Word8
loadAddr' idx
  | inGPURange idx = do
      gpu <- getGPU
      let (b , mgpu') = loadGPU gpu (fromIntegral idx)
      forM_ mgpu' putGPU
      return b
  | inInterruptRange (fromIntegral idx) = do
      s <- getInterrupt
      return $ loadInterrupt s (fromIntegral idx)
  | inTimerRange (fromIntegral idx) = do
      ts <- getTimerState
      return $ loadTimer ts (fromIntegral idx)
  | inCartridgeRange idx = GBT $ do
      cart <- asks gbCartridge
      liftIO $ loadCartridge cart (fromIntegral idx)
  | idx < 0xffff && inJoypadRange (fromIntegral idx) = do
      s <- readState gbJoypad
      return $ loadJoypad s (fromIntegral idx)
  | echoRam idx          = loadAddr' (idx - 0x2000)
  | 0xFEA0 <= idx && idx < 0xFF00 = return 0xff
  | otherwise = GBT $ do
      addrspace <- asks addressSpace
      liftIO $ V.read addrspace idx

{-# INLINE storeAddr' #-}
storeAddr' :: MonadIO m => Int -> Word8 -> GB m ()
storeAddr' idx b
  | idx == 0xff46 = do
      gpu <- readState gbGPU
      gpu' <- dmaTransfer (loadAddr . fromIntegral) ((b , 0x00) ^. word16) gpu
      writeState gbGPU gpu'
  | inGPURange idx = do
      gpu <- readState gbGPU
      let gpu' = storeGPU gpu (fromIntegral idx) b
      writeState gbGPU gpu'
  | inInterruptRange (fromIntegral idx) = do
      s <- getInterrupt
      putInterrupt $ storeInterrupt s (fromIntegral idx) b
  | inTimerRange (fromIntegral idx) = do
      ts <- getTimerState
      let ts' = storeTimer (fromIntegral idx) b ts
      putTimerState ts'
  | inCartridgeRange idx = GBT $ do
      cart <- asks gbCartridge
      liftIO $ storeCartridge (fromIntegral idx) b cart
  | idx < 0xffff && inJoypadRange (fromIntegral idx) = do
      s <- readState gbJoypad
      writeState gbJoypad $ storeJoypad (fromIntegral idx) b s
  | echoRam idx          = storeAddr' (idx - 0x2000) b
  | 0xFEA0 <= idx && idx < 0xFF00 = return ()
  | otherwise = GBT $ do
      addrspace <- asks addressSpace
      liftIO $ V.write addrspace idx b

instance MonadIO m => MonadEmulator (GB m) where
  storeReg r = storeAddr' (reg8ToIndex r)
  storeAddr addr = storeAddr' (fromIntegral addr)

  storeSP = writeState stackpointer
  storePC = writeState programCounter

  loadReg r = loadAddr' (reg8ToIndex r)
  loadAddr addr = loadAddr' (fromIntegral addr)

  loadSP = readState stackpointer
  loadPC = readState programCounter

  advCycles dt = GBT $ do
    c <- asks clock
    liftIO $ stToIO $ modifySTRef' c (+ dt)

  resetCycles = GBT $ do
    c <- asks clock
    liftIO $ stToIO $ do
      v <- readSTRef c
      writeSTRef c 0
      return v

  getTimerState = readState  gbTimer
  putTimerState = writeState gbTimer

  getGPU = readState  gbGPU
  putGPU = writeState gbGPU

  setHalt = writeState isHalted True
  clearHalt = writeState isHalted False
  halt = readState isHalted

  getInterrupt = readState  gbInterrupt
  putInterrupt = writeState gbInterrupt

  setStop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ writeSTRef s True

  stop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ readSTRef s

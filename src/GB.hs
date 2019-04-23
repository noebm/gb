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

import Control.Lens
import Control.Monad.ST
import Control.Monad.Reader

import MonadEmulator

import GPU.GPUState
import Interrupt.Interrupt
import Cartridge.Cartridge
import Joypad

data GBState s = GBState
  { addressSpace :: MVector s Word8
  , clock        :: STRef s Word
  , shouldStop   :: STRef s Bool

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
  memory <- V.replicate (0xFFFF + 0xC) 0x00
  GBState
    <$> pure memory
    <*> newSTRef 0
    <*> newSTRef False
    <*> newSTRef defaultInterruptState
    <*> newSTRef defaultGPUState
    <*> newSTRef defaultJoypadState
    <*> pure cart

runGB :: MonadIO m => CartridgeState RealWorld -> GB m a -> m a
runGB cart (GBT x) = do
  gbState <- liftIO $ stToIO $ makeGBState cart
  runReaderT x gbState

getJoypad :: MonadIO m => GB m JoypadState
getJoypad = GBT $ getSTRef gbJoypad

putJoypad :: MonadIO m => JoypadState -> GB m ()
putJoypad s = GBT $ putSTRef gbJoypad s

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
reg8index A = 1
reg8index F = 0
reg8index B = 3
reg8index C = 2
reg8index D = 5
reg8index E = 4
reg8index H = 7
reg8index L = 6

{-# INLINE ls8ToIndex #-}
ls8ToIndex :: LoadStore8 -> Int
ls8ToIndex (Register8 r) = rbase + reg8index r
  where rbase = 0x10000
ls8ToIndex (Addr8 addr) = fromIntegral addr

{-# INLINE ls16ToIndex #-}
ls16ToIndex :: LoadStore16 -> (Int,Int)
ls16ToIndex (Register16 r) = reg16decomp r & each +~ rbase
  where
  rbase = 0x10000
  {-# INLINE reg16decomp #-}
  reg16decomp AF = (F, A) & each %~ reg8index
  reg16decomp BC = (C, B) & each %~ reg8index
  reg16decomp DE = (E, D) & each %~ reg8index
  reg16decomp HL = (L, H) & each %~ reg8index
  reg16decomp PC = (0x8 , 0x9)
  reg16decomp SP = (0xA , 0xB)

ls16ToIndex (Addr16 addr) = (fromIntegral addr , fromIntegral addr + 1)

{-# INLINE echoRam #-}
echoRam :: Int -> Bool
echoRam addr = 0xE000 <= addr && addr < 0xFE00

{-# INLINE loadAddr #-}
loadAddr :: MonadIO m => Int -> GB m Word8
loadAddr idx
  | inGPURange idx = do
      gpu <- getGPU
      let (b , mgpu') = loadGPU gpu (fromIntegral idx)
      forM_ mgpu' putGPU
      return b
  | inInterruptRange (fromIntegral idx) = do
      s <- getInterrupt
      return $ loadInterrupt s (fromIntegral idx)
  | inCartridgeRange idx = GBT $ do
      cart <- asks gbCartridge
      liftIO $ loadCartridge cart (fromIntegral idx)
  | idx < 0xffff && inJoypadRange (fromIntegral idx) = GBT $ do
      s <- getSTRef gbJoypad
      return $ loadJoypad s (fromIntegral idx)
  | echoRam idx          = loadAddr (idx - 0x2000)
  | 0xFEA0 <= idx && idx < 0xFF00 = return 0xff
  | otherwise = GBT $ do
      addrspace <- asks addressSpace
      liftIO $ V.unsafeRead addrspace idx

getSTRef :: (MonadIO m, s ~ RealWorld) => (GBState s -> STRef s a) -> ReaderT (GBState s) m a
getSTRef f = liftIO . stToIO . readSTRef =<< asks f

putSTRef :: (MonadIO m, s ~ RealWorld) => (GBState s -> STRef s a) -> a -> ReaderT (GBState s) m ()
putSTRef f x = liftIO . stToIO . (`writeSTRef` x) =<< asks f

{-# INLINE storeAddr #-}
storeAddr :: MonadIO m => Int -> Word8 -> GB m ()
storeAddr idx b
  | inGPURange idx = GBT $ do
      gpu <- getSTRef gbGPU
      let gpu' = storeGPU gpu (fromIntegral idx) b
      putSTRef gbGPU gpu'
  | inInterruptRange (fromIntegral idx) = do
      s <- getInterrupt
      putInterrupt $ storeInterrupt s (fromIntegral idx) b
  | inCartridgeRange idx = GBT $ do
      cart <- asks gbCartridge
      liftIO $ storeCartridge (fromIntegral idx) b cart
  | idx < 0xffff && inJoypadRange (fromIntegral idx) = GBT $ do
      s <- getSTRef gbJoypad
      putSTRef gbJoypad $ storeJoypad (fromIntegral idx) b s
  | echoRam idx          = storeAddr (idx - 0x2000) b
  | 0xFEA0 <= idx && idx < 0xFF00 = return ()
  | otherwise = GBT $ do
      addrspace <- asks addressSpace
      liftIO $ V.unsafeWrite addrspace idx b

instance MonadIO m => MonadEmulator (GB m) where
  {-# INLINE store8 #-}
  store8 ls = storeAddr (ls8ToIndex ls)

  {-# INLINE store16 #-}
  store16 ls w =
    let (idx0, idx1) = ls16ToIndex ls
    in store16LE (storeAddr idx0) (storeAddr idx1) w

  {-# INLINE load8 #-}
  load8 ls = loadAddr (ls8ToIndex ls)

  {-# INLINE load16 #-}
  load16 ls =
    let (idx0, idx1) = ls16ToIndex ls
    in load16LE (loadAddr idx0) (loadAddr idx1)

  advCycles dt = GBT $ do
    c <- asks clock
    liftIO $ stToIO $ modifySTRef' c (+ dt)

  resetCycles = GBT $ do
    c <- asks clock
    liftIO $ stToIO $ do
      v <- readSTRef c
      writeSTRef c 0
      return v

  getGPU = GBT $ getSTRef gbGPU
  putGPU = GBT . putSTRef gbGPU

  getInterrupt = GBT $ getSTRef gbInterrupt
  putInterrupt = GBT . putSTRef gbInterrupt

  setStop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ writeSTRef s True

  stop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ readSTRef s

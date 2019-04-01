{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances #-}
module GB
  ( MonadEmulator(..)
  , GB
  , runGB
  , showRegisters
  , unsafeMemory
  )
where

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as VU
import qualified Data.ByteString as B
import Data.Vector.Unboxed.Mutable (MVector)
import Data.STRef
import Data.Word

import Text.Printf

import Control.Lens
import Control.Monad.ST
import Control.Monad.Reader

import MonadEmulator
import Cartridge
import VectorUtils

data GBState s = GBState
  { addressSpace :: MVector s Word8
  , clock        :: STRef s Word
  , shouldStop   :: STRef s Bool

  , gbCartridge   :: Cartridge
  , activeRomBank :: STRef s (Maybe Word8)

  , gbBankingMode :: STRef s Bool

  , gbEnableERam  :: STRef s Bool
  , activeRamBank :: STRef s (Maybe Word8)

  , ramBanks      :: MVector s Word8
  }

newtype GBT s m a = GBT (ReaderT (GBState s) m a)
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (GBT s m) where
  liftIO f = GBT $ liftIO f

type GB = GBT RealWorld

unsafeMemory :: Monad m => GBT s m (MVector s Word8)
unsafeMemory = GBT $ asks addressSpace


copyBankAux :: Int -> Int -> V.MVector s Word8 -> B.ByteString -> ST s ()
copyBankAux target k memory cart = do
  let bank = B.take 0x4000 $ B.drop (0x4000 * (k - 1)) cart
  VU.copy (V.slice target 0x4000 memory) $ byteStringToVector bank

makeGBState :: Cartridge -> ST s (GBState s)
makeGBState cart = do
  memory <- V.replicate (0xFFFF + 0xC) 0x00
  let copyBank k = copyBankAux (0x4000 * k) k memory (cartridgeData cart)
  copyBank 0
  copyBank 1
  externalRam <- V.replicate (0x2000 * fromIntegral (cartridgeRamBanks cart)) 0x00
  GBState
    <$> pure memory
    <*> newSTRef 0
    <*> newSTRef False
    <*> pure cart
    <*> newSTRef Nothing
    <*> newSTRef False
    <*> newSTRef False
    <*> newSTRef Nothing
    <*> pure externalRam

runGB :: MonadIO m => Cartridge -> GB m a -> m a
runGB cart (GBT x) = do
  gbState <- liftIO $ stToIO $ makeGBState cart
  runReaderT x gbState

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

instance MonadIO m => MonadEmulator (GB m) where
  {-# INLINE store8 #-}
  store8 ls b = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ V.unsafeWrite addrspace (ls8ToIndex ls) b

  {-# INLINE store16 #-}
  store16 ls w = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ do
      let (idx0, idx1) = ls16ToIndex ls
      store16LE
        (V.unsafeWrite addrspace idx0)
        (V.unsafeWrite addrspace idx1)
        w

  {-# INLINE load8 #-}
  load8 ls = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ V.unsafeRead addrspace (ls8ToIndex ls)

  {-# INLINE load16 #-}
  load16 ls = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ do
      let (idx0, idx1) = ls16ToIndex ls
      load16LE
        (V.unsafeRead addrspace idx0)
        (V.unsafeRead addrspace idx1)

  advCycles dt = GBT $ do
    c <- asks clock
    liftIO $ stToIO $ modifySTRef' c (+ dt)

  resetCycles = GBT $ do
    c <- asks clock
    liftIO $ stToIO $ do
      v <- readSTRef c
      writeSTRef c 0
      return v

  setStop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ writeSTRef s True

  stop = GBT $ do
    s <- asks shouldStop
    liftIO $ stToIO $ readSTRef s

  modifyRomBank f = GBT $ do
    romBank <- liftIO . stToIO . readSTRef =<< asks activeRomBank
    forM_ romBank $ \k -> do
      let k' = f k
      let idx = fromIntegral k'
      memory <- asks addressSpace
      cart   <- asks (cartridgeData . gbCartridge)
      cartBanks <- asks (cartridgeRomBanks . gbCartridge)
      if idx < fromIntegral cartBanks
        then liftIO $ stToIO $ copyBankAux 0x4000 idx memory cart
        else error $ printf "Access to rom bank out of range %d" idx

  selectRamBank k = do
    storeERAM
    let idx = fromIntegral k
    active <- GBT $ asks activeRamBank
    cartBanks <- GBT $ asks (cartridgeRamBanks . gbCartridge)
    f <- GBT $ liftIO . stToIO . readSTRef =<< asks gbEnableERam
    if f && idx < fromIntegral cartBanks
      then do
      liftIO $ stToIO $ writeSTRef active (Just idx)
      loadERAM
      else error $ printf "Access to ram bank out of range (Enabled: %d) %d" (fromEnum f) idx

  setRamBank f = GBT $
    liftIO . stToIO . (`writeSTRef` f) =<< asks gbEnableERam

accessERAM :: MonadIO m
           => GBT RealWorld m (V.MVector RealWorld Word8, Maybe (V.MVector RealWorld Word8))
accessERAM = GBT $ do
  memory <- asks addressSpace
  ram    <- asks ramBanks
  currentRamBank <- liftIO . stToIO . readSTRef =<< asks activeRamBank
  let eram = V.slice 0xA000 0x2000 memory
  let bankslice = do
        bank <- currentRamBank
        return $ V.slice (0x2000 * fromIntegral bank) 0x2000 ram
  return (eram, bankslice)

storeERAM :: MonadIO m => GBT RealWorld m ()
storeERAM = do
  (eram , bankslice) <- accessERAM
  liftIO $ stToIO $ forM_ bankslice $ \b -> V.copy b eram

loadERAM :: MonadIO m => GBT RealWorld m ()
loadERAM = do
  (eram , bankslice) <- accessERAM
  liftIO $ stToIO $ forM_ bankslice $ \b -> V.copy eram b


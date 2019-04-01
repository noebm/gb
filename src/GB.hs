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
import Data.Vector.Unboxed.Mutable (MVector)
import Data.STRef
import Data.Word

import Text.Printf

import Control.Lens
import Control.Monad.ST
import Control.Monad.Reader

import MonadEmulator
import Cartridge

data GBState s = GBState
  { addressSpace :: MVector s Word8
  , clock        :: STRef s Word
  , shouldStop   :: STRef s Bool

  , gbCartridge  :: Cartridge
  , romBank      :: STRef s (Maybe Word8)
  , ramBank      :: STRef s (Maybe Word8)
  }

newtype GBT s m a = GBT (ReaderT (GBState s) m a)
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (GBT s m) where
  liftIO f = GBT $ liftIO f

type GB = GBT RealWorld

unsafeMemory :: Monad m => GBT s m (MVector s Word8)
unsafeMemory = GBT $ asks addressSpace

runGB :: MonadIO m => Cartridge -> GB m a -> m a
runGB cart (GBT x) = do
  gbState <- liftIO $ stToIO $
    GBState
      <$> V.replicate (0xFFFF + 0xD) 0x00
      <*> newSTRef 0
      <*> newSTRef False
      <*> pure cart
      <*> newSTRef Nothing
      <*> newSTRef Nothing
  runReaderT x gbState

reg8index :: Reg8 -> Int
reg8index A = 1
reg8index F = 0
reg8index B = 3
reg8index C = 2
reg8index D = 5
reg8index E = 4
reg8index H = 7
reg8index L = 6

reg16index :: Reg16 -> Int
reg16index AF = 0
reg16index BC = 2
reg16index DE = 4
reg16index HL = 6
reg16index PC = 8
reg16index SP = 10

ls8ToIndex :: LoadStore8 -> Int
ls8ToIndex (Register8 r) = rbase + reg8index r
  where rbase = 0x10000
ls8ToIndex (Addr8 addr) = fromIntegral addr

ls16ToIndex :: LoadStore16 -> Int
ls16ToIndex (Register16 r) = rbase + reg16index r
  where rbase = 0x10000
ls16ToIndex (Addr16 addr) = fromIntegral addr

instance MonadIO m => MonadEmulator (GB m) where
  {-# INLINE store8 #-}
  store8 ls b = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ V.unsafeWrite addrspace (ls8ToIndex ls) b

  {-# INLINE store16 #-}
  store16 ls w = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ do
      let idx = ls16ToIndex ls
      let (h , l) = w ^. from word16
      V.unsafeWrite addrspace idx l
      V.unsafeWrite addrspace (idx + 1) h

  {-# INLINE load8 #-}
  load8 ls = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ V.unsafeRead addrspace (ls8ToIndex ls)

  {-# INLINE load16 #-}
  load16 ls = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ do
      let idx = ls16ToIndex ls
      l <- V.unsafeRead addrspace idx
      h <- V.unsafeRead addrspace (idx + 1)
      return $ (h , l) ^. word16

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

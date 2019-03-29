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

data GBState s = GBState
  { addressSpace :: MVector s Word8
  , clock        :: STRef s Word
  }

newtype GBT s m a = GBT (ReaderT (GBState s) m a)
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (GBT s m) where
  liftIO f = GBT $ liftIO f

type GB = GBT RealWorld

unsafeMemory :: Monad m => GBT s m (MVector s Word8)
unsafeMemory = GBT $ asks addressSpace

runGB :: MonadIO m => GB m a -> m a
runGB (GBT x) = do
  gbState <- liftIO $ stToIO $ do
    addr <- V.replicate (0xFFFF + 0xD) 0x00
    t <- newSTRef 0
    return $ GBState addr t
  runReaderT x gbState


reg8index :: Reg8 -> Int
reg8index A = 0
reg8index F = 1
reg8index B = 2
reg8index C = 3
reg8index D = 4
reg8index E = 5
reg8index H = 6
reg8index L = 7

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
  store8 ls b = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ V.write addrspace (ls8ToIndex ls) b

  store16 ls w = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ do
      let idx = ls16ToIndex ls
      let (h , l) = w ^. from word16
      V.write addrspace idx h
      V.write addrspace (idx + 1) l

  load8 ls = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ V.read addrspace (ls8ToIndex ls)
  load16 ls = GBT $ do
    addrspace <- asks addressSpace
    liftIO $ do
      let idx = ls16ToIndex ls
      h <- V.read addrspace idx
      l <- V.read addrspace (idx + 1)
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


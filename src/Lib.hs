{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
--     ( someFunc
--     )
where

import qualified Data.ByteString as B
-- import qualified Data.Vector.Storable as V
import Data.Word
import Data.Int
import Data.Bits
import Data.Bits.Lens
import Control.Lens.Unsound (lensProduct)

import Text.Printf

import Control.Lens
import Control.Monad.State
import Data.Foldable

import MonadEmulator
import Instruction

data CPUState = CPUState
  -- special registers
  { _regPC :: Word16 -- program counter
  , _regSP :: Word16 -- stack pointer
  -- normal registers
  , _regA :: Word8
  , _regF :: Word8 -- flags
  , _regB :: Word8
  , _regC :: Word8
  , _regD :: Word8
  , _regE :: Word8
  , _regH :: Word8
  , _regL :: Word8
  } deriving (Eq)

instance Show CPUState where
  show (CPUState pc sp a f b c d e h l) =
    printf "PC: %4x, SP: %4x, A: %2x, F: %2x, B: %2x, C: %2x, D: %2x, E: %2x, H: %2x, L: %2x" pc sp a f b c d e h l

newCPUState :: CPUState
newCPUState = CPUState 0 0 0 0 0 0 0 0 0 0

makeLenses ''CPUState

reg8lens :: Reg8 -> Lens' CPUState Word8
reg8lens A = regA
reg8lens B = regB
reg8lens C = regC
reg8lens D = regD
reg8lens E = regE
reg8lens F = regF
reg8lens H = regH
reg8lens L = regL

reg16lens :: Reg16 -> Lens' CPUState Word16
reg16lens AF = lensProduct regA regF . word16
reg16lens BC = lensProduct regB regC . word16
reg16lens DE = lensProduct regD regE . word16
reg16lens HL = lensProduct regH regL . word16
reg16lens PC = regPC
reg16lens SP = regSP

-- data Memory = Memory
--   { getMemory :: B.ByteString -- V.Vector Word8
--   }
type Memory = B.ByteString

memoryBootRom :: IO Memory
memoryBootRom = do
  let bootStrapName = "DMG_ROM.bin"
  bootStrapRom <- B.readFile $ "./" ++ bootStrapName
  return $ bootStrapRom

data GBState = GBState
  { _cpuState :: CPUState
  , _memory :: Memory
  , _timer :: Word
  }
  deriving (Show)

newGBState m = GBState newCPUState m 0

makeLenses ''GBState

newtype GBT m a = GBT (StateT GBState m a)
  deriving (Functor, Applicative, Monad, MonadState GBState, MonadIO)

runGB :: Monad m => GBT m a -> GBState -> m a
runGB (GBT act) s = evalStateT act s

instance Monad m => MonadEmulator (GBT m) where
  -- store8 :: LoadStore8 -> Word8 -> m ()
  store8 (Register8 r) = assign (cpuState.reg8lens r)
  store8 (Addr8 addr)  = assign (memory.singular (ix (fromIntegral addr)))

  load8 (Register8 r) = use (cpuState.reg8lens r)
  load8 (Addr8 addr)  = use (memory.singular(ix (fromIntegral addr)))

  store16 (Register16 r) dw = assign (cpuState.reg16lens r) dw
  store16 (Addr16 addr) dw = do
    let (hw , lw) = dw ^. from word16
    store8 (Addr8 addr) lw
    store8 (Addr8 $ addr + 1) hw

  load16 (Register16 r) = use (cpuState.reg16lens r)
  load16 (Addr16 addr) = do
    lw <- load8 (Addr8 addr)
    hw <- load8 (Addr8 $ addr + 1)
    return $ (hw , lw) ^. word16

  advCycles dt = timer += dt
  getCycles = use timer


someFunc :: IO ()
someFunc = do
  rom <- memoryBootRom
  let s = newGBState rom
  (`runGB` s) $ do
    forM_ [1..5] $ \ k -> do
      b <- immediate8
      liftIO . putStrLn $ "Instruction: " ++ hexbyte b
      advCycles =<< instruction b
      -- liftIO . putStrLn $ "Register state: "
      liftIO . print =<< use cpuState
    -- advCycles =<< instruction =<< immediate8
    -- liftIO . print =<< use cpuState
  return ()

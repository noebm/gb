{-# LANGUAGE GADTs #-}
module Cartridge.CartridgeState where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Cartridge.CartridgeConfig
-- import Cartridge.RamBank

-- import qualified Data.ByteString as B
import Data.Word
import Data.Bits

import Control.Comonad.Store
import Control.Comonad.Env

-- import Control.Comonad

data RomState = RomState
  { getRomBank      :: RomBank
  , getRomBankIndex :: Int
  }

data RamState = RamState
  { ramBanks      :: V.Vector RamBank
  , activeRamBank :: Int
  }

data CartridgeState = CartridgeState
  { romBank  :: RomState
  , ramState :: RamState

  , bootRomEnabled :: Bool
  }

-- getRomBankInternal :: V.Vector RomBank -> Int -> RomBank
-- getRomBankInternal v idx = v V.! idx

type RomState' = Env (V.Vector RomBank)

selectRomBank :: (Int -> Int) -> RomState' Int -> RomState' Int
selectRomBank = fmap

getRomBank' :: RomState' Int -> RomBank
getRomBank' s = let idx = extract s in asks (V.! idx) s

loadRomBank :: Word16 -> RomState' Int -> Word8
loadRomBank addr s = getRom (fromIntegral $ addr .&. 0x3fff) $ getRomBank' s

type RamState' = Store RamState

updateRamVector :: Int -> (RamBank -> RamBank) -> V.Vector RamBank -> V.Vector RamBank
updateRamVector idx upd = V.modify (\x -> VM.write x idx . upd =<< VM.read x idx)

storeRamBank :: Word16 -> Word8 -> Store Int (V.Vector RamBank) -> Store Int (V.Vector RamBank)
storeRamBank addr b c = set (pos c) addr b <<= c

set :: Comonad w => Int -> Word16 -> Word8 -> w (V.Vector RamBank) -> V.Vector RamBank
set idx addr b = updateRamVector idx (setRam (fromIntegral $ addr .&. 0x3fff) b) . extract
    -- seeks (updateRamVector idx (setRam (fromIntegral $ addr .&. 0x3fff) b)) c

getRamBank' :: Store Int (V.Vector RamBank) -> RamBank
getRamBank' c = extract c V.! pos c

-- loadRamBank :: Word16 -> Store (V.Vector RamBank) Int -> Word8
-- loadRamBank addr c = getRam (fromIntegral $ addr .&. 0x3fff) . getRamBank' c

setRamBank :: Int -> Store Int a -> Store Int a -- Store (V.Vector RamBank) a -> Store (V.Vector RamBank) Int
setRamBank  = seek

-- updateRamBank :: Int -> Store (V.Vector RamBank) Int -> Store (V.Vector RamBank) Int
-- updateRamBank idx' c =
--   let idx = extract c
--   in c -- seeks (\ s -> V.modify _) c

-- type BankState f a = f [ a ] a
-- 
-- type RomBankState = BankState (->) RomBank -- use env comonad
-- 
-- type RamBankState = BankState (,) RamBank -- use store comonad

-- getRamBank :: Store (Controller RamBank) RamBank
-- getRamBank = store ()

-- getRomBank :: Env (Controller RomBank) RomBank -> RomBank
-- getRomBank = extract

-- updateRomBank :: ([ RomBank ] -> RomBank) -> RomBankState -> RomBankState
-- updateRomBank f' _ = f'
-- 
-- updateRamBank :: ([ RamBank ] -> RamBank) -> (RamBank -> [ RamBank ] -> [ RamBank ])
--               -> RamBankState -> RamBankState
-- updateRamBank f store (s , x) = let s' = store x s in (s' , f s')

-- access :: BankState f a -> a
-- update :: BankState f a -> (BankState f a -> a) -> (a -> BankState f a) -> 

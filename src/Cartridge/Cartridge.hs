module Cartridge.Cartridge where

import Cartridge.Header
import Cartridge.Controller

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Data.Bits

import Control.Monad.Primitive
import Control.Monad

import VectorUtils

data CartridgeState s = CartridgeState
  { header :: Header
  , bootrom :: Maybe BootRom
  , romBanks :: RomBank s
  , ramBanksEnable :: Bool
  , ramBanks :: RamBank s
  }

newtype BootRom = BootRom (VU.Vector Word8)

loadBootRom :: Word16 -> BootRom -> Word8
loadBootRom addr (BootRom xs) = xs VU.! fromIntegral addr

loadCartridge :: PrimMonad m => CartridgeState (PrimState m) -> Word16 -> m Word8
loadCartridge s addr
  | addr <= 0xff  = maybe (loadRom (romBanks s) addr) (return . loadBootRom addr) (bootrom s)
  | addr < 0x8000 = loadRom (romBanks s) addr
  | 0x8000 <= addr && addr < 0xC000 = if ramBanksEnable s
    then loadRam (ramBanks s) addr
    else return 0xff
  | otherwise = error "loadCartridge: out of range"

newtype Rom = Rom (VU.Vector Word8)

readRom :: FilePath -> IO (Either String Rom)
readRom fp = do
  bytes <- B.readFile fp
  let vs = byteStringToVector bytes
  return $ do
    when (VU.length vs < 0x8000) $ Left "readRom: file too short"
    when (VU.length vs .&. 0x3fff == 0) $ Left "readRom: file has invalid length"
    return $ Rom vs

readBootRom :: FilePath -> IO (Either String BootRom)
readBootRom fp = do
  bytes <- B.readFile fp
  let vs = byteStringToVector bytes
  return $ do
    when (VU.length vs /= 0x100) $ Left "readBootRom: invalid length"
    return $ BootRom vs

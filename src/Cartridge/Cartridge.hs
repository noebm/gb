module Cartridge.Cartridge where

import qualified Cartridge.Header as Header
import Cartridge.Controller

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Data.Bits
import Data.Maybe

import Control.Monad.Primitive
import Control.Monad

import VectorUtils

data CartridgeState s = CartridgeState
  { header :: Maybe Header.Header
  , bootrom :: Maybe BootRom
  , romBanks :: RomBank s
  , ramBanksEnable :: Bool
  , ramBanks :: RamBank s
  }

defaultCartridge :: PrimMonad m => m (CartridgeState (PrimState m))
defaultCartridge = do
  rom <- defaultRomBank
  ram <- emptyRamBank
  return $ CartridgeState
    { header = Nothing
    , bootrom = Nothing
    , romBanks = rom
    , ramBanksEnable = False
    , ramBanks = ram
    }

loadBootRom :: Word16 -> BootRom -> Word8
loadBootRom addr (BootRom xs) = xs VU.! fromIntegral addr

{-# INLINE inCartridgeRange #-}
inCartridgeRange :: Word16 -> Bool
inCartridgeRange addr
  = addr < 0x8000    -- cartridge
  || inRamRange addr -- ram banks
  || addr == 0xff50  -- boot rom disable

loadCartridge :: PrimMonad m => CartridgeState (PrimState m) -> Word16 -> m Word8
loadCartridge s addr
  | addr <= 0xff  = maybe (loadRom (romBanks s) addr) (return . loadBootRom addr) (bootrom s)
  | addr < 0x8000 = loadRom (romBanks s) addr
  | inRamRange addr = if ramBanksEnable s
    then loadRam (ramBanks s) addr
    else return 0xff
  | addr == 0xff50 = return $ fromIntegral . fromEnum . isJust $ bootrom s
  | otherwise = error "loadCartridge: out of range"

storeCartridge :: PrimMonad m => Word16 -> Word8 -> CartridgeState (PrimState m) -> m (CartridgeState (PrimState m))
storeCartridge addr b c
  | addr < 0x8000 = error "storeCartridge: address < 0x8000 not implemented"
  | inRamRange addr = storeRam addr b (ramBanks c) >> return c
  | addr == 0xff50 = return $ if b `testBit` 0 then c { bootrom = Nothing } else c
  | otherwise = error "storeCartridge: out of range"

makeCartridge :: PrimMonad m => Maybe BootRom -> Rom -> m (CartridgeState (PrimState m))
makeCartridge boot (Rom h xs) = do
  rom <- makeRomBanks xs
  ram <- newRamBanks 0
  return $ CartridgeState (Just h) boot rom False ram

data Rom = Rom Header.Header (VU.Vector Word8)

readRom :: FilePath -> IO (Either String Rom)
readRom fp = do
  bytes <- B.readFile fp
  let vs = byteStringToVector bytes
  return $ do
    when (VU.length vs < 0x8000) $ Left "readRom: file too short"
    when (VU.length vs .&. 0x3fff == 0) $ Left "readRom: file has invalid length"
    h <- maybe (Left "readRom: reader parsing failed") Right $ Header.header bytes
    return (Rom h vs)

newtype BootRom = BootRom (VU.Vector Word8)

readBootRom :: FilePath -> IO (Either String BootRom)
readBootRom fp = do
  bytes <- B.readFile fp
  let vs = byteStringToVector bytes
  return $ do
    when (VU.length vs /= 0x100) $ Left "readBootRom: invalid length"
    return $ BootRom vs

module Cartridge.Cartridge where

import qualified Cartridge.Header as Header
import Cartridge.Header ( MBCType(..) )
import Cartridge.Controller

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Data.Bits
import Data.Maybe
import Text.Printf

import Data.STRef

import Control.Lens
import Control.Monad.Primitive
import Control.Monad

import VectorUtils

data CartridgeState s = CartridgeState
  { header :: Header.Header
  , bootrom :: Maybe BootRom
  , bootromEnable :: STRef s Bool
  , romBanks :: RomBank s
  , ramBanksEnable :: STRef s Bool
  , ramBanks :: RamBank s
  }

defaultCartridge :: PrimMonad m => m (CartridgeState (PrimState m))
defaultCartridge = do
  rom <- defaultRomBank
  ram <- emptyRamBank
  ramEnable <- stToPrim $ newSTRef False
  bootEnable <- stToPrim $ newSTRef False
  return $ CartridgeState
    { header = error "no default header implementation"
    , bootrom = Nothing
    , bootromEnable = bootEnable
    , romBanks = rom
    , ramBanksEnable = ramEnable
    , ramBanks = ram
    }

makeCartridge :: PrimMonad m => Maybe BootRom -> Rom -> m (CartridgeState (PrimState m))
makeCartridge boot (Rom h xs) = do
  rom <- makeRomBanks xs
  ram <- newRamBanks 0

  ramEnable <- stToPrim $ newSTRef False
  bootEnable <- stToPrim $ newSTRef (isJust boot)

  return $ CartridgeState h boot bootEnable rom ramEnable ram

loadBootRom :: Word16 -> BootRom -> Word8
loadBootRom addr (BootRom xs) = xs VU.! fromIntegral addr

{-# INLINE inCartridgeRange #-}
inCartridgeRange :: (Num a, Ord a, Eq a) => a -> Bool
inCartridgeRange addr
  = addr < 0x8000    -- cartridge
  || inRamRange addr -- ram banks
  || addr == 0xff50  -- boot rom disable

loadCartridge :: PrimMonad m => CartridgeState (PrimState m) -> Word16 -> m Word8
loadCartridge s addr
  | addr <= 0xff  = do
      e <- stToPrim $ readSTRef (bootromEnable s)
      maybe (loadRom (romBanks s) addr) return $ do
        guard e
        loadBootRom addr <$> bootrom s
  | 0xff < addr && addr < 0x8000 = loadRom (romBanks s) addr
  | inRamRange addr = do
      e <- stToPrim $ readSTRef (ramBanksEnable s)
      if e
        then loadRam (ramBanks s) addr
        else return 0xff
  | addr == 0xff50 =
      fromIntegral . fromEnum . not <$> stToPrim ( readSTRef $ bootromEnable s)
  | otherwise = error "loadCartridge: out of range"

storeCartridge :: PrimMonad m => Word16 -> Word8 -> CartridgeState (PrimState m) -> m ()
storeCartridge addr b c
  | addr < 0x8000 = storeMBC (view Header.mbcType $ Header.headerType (header c)) addr b c
  | inRamRange addr = storeRam addr b (ramBanks c)
  | addr == 0xff50 = stToPrim $ writeSTRef (bootromEnable c) (b == 0)
  | otherwise = error "storeCartridge: out of range"

data Rom = Rom Header.Header (VU.Vector Word8)

readRom :: FilePath -> IO (Either String Rom)
readRom fp = do
  bytes <- B.readFile fp
  let vs = byteStringToVector bytes
  return $ do
    when (VU.length vs < 0x8000) $ Left "readRom: file too short"
    when (VU.length vs `mod` 0x4000 /= 0) $ Left $ printf "readRom: file has invalid length 0x%x" (VU.length vs)
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

storeMBC :: PrimMonad m => MBCType -> Word16 -> Word8 -> CartridgeState (PrimState m) -> m () -- RomBank s
storeMBC OnlyROM addr _ _
  | addr < 0x8000 = return ()
  | otherwise = error "storeMBC: out of range"
storeMBC MBC1 addr b s
  | addr < 0x2000 = error "storeMBC: MBC1 enable ram"
  | addr < 0x4000 = error "storeMBC: MBC1 lower rom bits"
  | addr < 0x6000 = error "storeMBC: MBC1 higher rom/ram bits"
  | addr < 0x8000 = error "storeMBC: MBC1 rom/ram mode select"
  | otherwise = error "storeMBC: out of range"

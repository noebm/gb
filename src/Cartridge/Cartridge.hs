module Cartridge.Cartridge where

import qualified Cartridge.Header as Header
import Cartridge.Bank.RomBank
import Cartridge.Bank.RamBank
import Cartridge.BootRom
import Cartridge.MemoryBankController

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Data.Maybe
import Text.Printf

import Control.Lens
import Control.Monad

import Utilities.Vector

data CartridgeState = CartridgeState
  { header :: Header.Header
  , bootrom :: Maybe BootRom
  , bootromEnable :: Bool
  , romBanks :: RomBank
  , ramBanks :: Maybe RamBank
  , mbc :: MemoryBankController
  }

defaultCartridge :: CartridgeState
defaultCartridge =
  CartridgeState
  { header = error "no default header implementation"
  , bootrom = Nothing
  , bootromEnable = False
  , romBanks = defaultRomBank

  , mbc = defaultMBC

  , ramBanks = Nothing
  }

makeCartridge :: Maybe BootRom -> Rom -> CartridgeState
makeCartridge boot (Rom h xs) =
  CartridgeState
    { header = h
    , bootrom = boot
    , bootromEnable = isJust boot
    , romBanks = makeRomBanks (Header.headerRomBanks h) xs

    , mbc = memoryBankController (view Header.mbcType $ Header.headerType h)

    , ramBanks = newRamBanks (fromIntegral $ Header.headerRamBanks h)
    }

loadCartridge :: Word16 -> CartridgeState -> Word8
loadCartridge addr s
  | bootromEnable s && addr <= 0xff = maybe 0xff (loadBootRom addr) $ bootrom s
  | addr < 0x8000 = loadRom (romBanks s) addr
  | otherwise = error "loadCartridge: address not in cartridge range"

loadCartridgeRAM :: Word16 -> CartridgeState -> Word8
loadCartridgeRAM addr s = maybe 0xff id $ do
  guard $ ramAccessible $ mbc s
  banks <- ramBanks s
  return $ loadRam banks addr

loadCartridgeBootRomRegister :: CartridgeState -> Word8
loadCartridgeBootRomRegister s = fromIntegral . fromEnum . not $ (bootromEnable s)

storeCartridge :: Word16 -> Word8 -> CartridgeState -> CartridgeState
storeCartridge addr b c =
  let (mbc' , rom', ram') = storeMBC addr b (mbc c, romBanks c, ramBanks c)
  in c { mbc = mbc' , romBanks = rom', ramBanks = ram' }

storeCartridgeRAM :: Word16 -> Word8 -> CartridgeState -> CartridgeState
storeCartridgeRAM addr b c = maybe c (\ram -> c { ramBanks = Just ram }) $ do
  guard $ ramAccessible $ mbc c
  storeRam addr b <$> ramBanks c

storeCartridgeBootRomRegister :: Word8 -> CartridgeState -> CartridgeState
storeCartridgeBootRomRegister b c = c { bootromEnable = b == 0 }

data Rom = Rom Header.Header (VU.Vector Word8)

readRom :: FilePath -> IO (Either String Rom)
readRom fp = do
  bytes <- B.readFile fp
  let vs = byteStringToVector bytes
  return $ do
    when (VU.length vs < 0x8000) $ Left "readRom: file too short"
    when (VU.length vs `mod` 0x4000 /= 0) $ Left $ printf "readRom: file has invalid length 0x%x" (VU.length vs)
    h <- Header.header bytes
    return (Rom h vs)

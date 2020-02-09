module Cartridge.Cartridge where

import qualified Cartridge.Header as Header
import Cartridge.Header ( MBCType(..) )
import Cartridge.Bank.RomBank
import Cartridge.Bank.RamBank
import Cartridge.BootRom

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Word
import Data.Bits
import Data.Maybe
import Text.Printf

import Control.Lens
import Control.Monad

import Utilities.Vector

data MBC1Mode = MBC1_RomMode | MBC1_RamMode

data CartridgeState = CartridgeState
  { header :: Header.Header
  , bootrom :: Maybe BootRom
  , bootromEnable :: Bool
  , romBanks :: RomBank

  , mbc1mode :: MBC1Mode
  , highBits :: Word8 -- 2 bit containing bit 5,6 of rom index or 0,1 of ram index
  , lowerRomBankBits :: Word8

  , ramBanksEnable :: Bool
  , ramBanks :: Maybe RamBank
  }

defaultCartridge :: CartridgeState
defaultCartridge =
  CartridgeState
  { header = error "no default header implementation"
  , bootrom = Nothing
  , bootromEnable = False
  , romBanks = defaultRomBank

  , mbc1mode = MBC1_RomMode
  , highBits = 0x00
  , lowerRomBankBits = 0x00

  , ramBanksEnable = False
  , ramBanks = Nothing
  }

makeCartridge :: Maybe BootRom -> Rom -> CartridgeState
makeCartridge boot (Rom h xs) =
  CartridgeState
    { header = h
    , bootrom = boot
    , bootromEnable = isJust boot
    , romBanks = makeRomBanks xs

    , mbc1mode = MBC1_RomMode
    , highBits = 0x00
    , lowerRomBankBits = 0x00

    , ramBanksEnable = False
    , ramBanks = newRamBanks (fromIntegral $ Header.headerRamBanks h)
    }

loadCartridge :: Word16 -> CartridgeState -> Word8
loadCartridge addr s
  | addr <= 0xff  =
      let aux = (`loadRom` addr) (romBanks s)
      in maybe aux id $ do
        guard (bootromEnable s)
        loadBootRom addr <$> (bootrom s)
  | addr < 0x8000 = loadRom (romBanks s) addr
  | otherwise = error "loadCartridge: address not in cartridge range"

loadCartridgeRAM :: Word16 -> CartridgeState -> Word8
loadCartridgeRAM addr s = maybe 0xff id $ do
  guard (ramBanksEnable s)
  banks <- ramBanks s
  return $ loadRam banks addr

loadCartridgeBootRomRegister :: CartridgeState -> Word8
loadCartridgeBootRomRegister s = fromIntegral . fromEnum . not $ (bootromEnable s)

storeCartridge :: Word16 -> Word8 -> CartridgeState -> CartridgeState
storeCartridge addr b c = storeMBC (view Header.mbcType $ Header.headerType (header c)) addr b c

storeCartridgeRAM :: Word16 -> Word8 -> CartridgeState -> CartridgeState
storeCartridgeRAM addr b c = c { ramBanks = storeRam addr b <$> ramBanks c }

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
    h <- maybe (Left "readRom: reader parsing failed") Right $ Header.header bytes
    return (Rom h vs)

romHighBits :: CartridgeState -> Word8
romHighBits s = case mbc1mode s of
  MBC1_RamMode -> 0x00
  MBC1_RomMode -> (highBits s .&. 0x03) `shiftL` 5

romBankIndex :: CartridgeState -> Word8
romBankIndex s =
  let lo = lowerRomBankBits s .&. 0x1f
      hi = romHighBits s
  in hi .|. lo

ramBankIndex :: CartridgeState -> Word8
ramBankIndex s = case mbc1mode s of
  MBC1_RamMode -> highBits s .&. 0x03
  MBC1_RomMode -> 0x00

updateBanks :: CartridgeState -> CartridgeState
updateBanks s = s
  { romBanks = selectRomBank (fromIntegral $ romBankIndex s) (romBanks s)
  , ramBanks = selectRamBank (fromIntegral $ ramBankIndex s) <$> ramBanks s
  }

storeMBC :: MBCType -> Word16 -> Word8 -> CartridgeState -> CartridgeState
storeMBC OnlyROM addr _ s
  | addr < 0x8000 = s
  | otherwise = error "storeMBC: out of range"
storeMBC MBC1 addr b s
  | addr < 0x2000 = s { ramBanksEnable = b .&. 0xF == 0xA }
  | addr < 0x4000 =
    let s' = s { lowerRomBankBits = let x = b .&. 0x1f in if x == 0 then 1 else x }
    in updateBanks s'
  | addr < 0x6000 =
    let s' = s { highBits = b .&. 0x03 }
    in updateBanks s'
  | addr < 0x8000 =
    let s' = s { mbc1mode = if b `testBit` 0 then MBC1_RomMode else MBC1_RamMode }
    in updateBanks s'
  | otherwise = error "storeMBC: out of range"

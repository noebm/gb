module Cartridge.Cartridge where

import qualified Cartridge.Header as Header
import Cartridge.Header ( MBCType(..) )
import Cartridge.Controller
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


data CartridgeState = CartridgeState
  { header :: Header.Header
  , bootrom :: Maybe BootRom
  , bootromEnable :: Bool
  , romBanks :: RomBank
  , romBankIndex :: Int
  , ramBanksEnable :: Bool
  , ramBanks :: RamBank
  }

defaultCartridge :: CartridgeState
defaultCartridge =
  CartridgeState
  { header = error "no default header implementation"
  , bootrom = Nothing
  , bootromEnable = False
  , romBanks = defaultRomBank
  , romBankIndex = 1
  , ramBanksEnable = False
  , ramBanks = emptyRamBank
  }

makeCartridge :: Maybe BootRom -> Rom -> CartridgeState
makeCartridge boot (Rom h xs) =
  CartridgeState
    { header = h
    , bootrom = boot
    , bootromEnable = isJust boot
    , romBanks = makeRomBanks xs
    , romBankIndex = 1
    , ramBanksEnable = False
    , ramBanks = newRamBanks 0
    }

{-# INLINE inCartridgeRange #-}
inCartridgeRange :: (Num a, Ord a, Eq a) => a -> Bool
inCartridgeRange addr
  = addr < 0x8000    -- cartridge
  || inRamRange addr -- ram banks
  || addr == 0xff50  -- boot rom disable

loadCartridge :: CartridgeState -> Word16 -> Word8
loadCartridge s addr
  | addr <= 0xff  =
      let aux = (`loadRom` addr) (romBanks s)
      in maybe aux id $ do
        guard (bootromEnable s)
        loadBootRom addr <$> (bootrom s)
  | 0xff < addr && addr < 0x8000 = loadRom (romBanks s) addr
  | inRamRange addr =
      if ramBanksEnable s
        then
        (`loadRam` addr) (ramBanks s)
        else 0xff
  | addr == 0xff50 =
      fromIntegral . fromEnum . not $ (bootromEnable s)
  | otherwise = error "loadCartridge: out of range"

storeCartridge :: Word16 -> Word8 -> CartridgeState -> CartridgeState
storeCartridge addr b c
  | addr < 0x8000 = storeMBC (view Header.mbcType $ Header.headerType (header c)) addr b c
  | inRamRange addr = c { ramBanks = storeRam addr b (ramBanks c) }
  | addr == 0xff50 = c { bootromEnable = b == 0 }
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

storeMBC :: MBCType -> Word16 -> Word8 -> CartridgeState -> CartridgeState
storeMBC OnlyROM addr _ s
  | addr < 0x8000 = s
  | otherwise = error "storeMBC: out of range"
storeMBC MBC1 addr b s
  | addr < 0x2000 = s { ramBanksEnable = b .&. 0xF == 0xA }
  | addr < 0x4000 =
      let b' = let x = b .&. 0x1f in if x == 0 then 1 else x
          idx' = (\idx -> (idx .&. 0xe0) .|. fromIntegral b') (romBankIndex s)
      in s { romBankIndex = idx'
           , romBanks = selectRomBank idx' (romBanks s)
           }
  | addr < 0x6000 = error "storeMBC: MBC1 higher rom/ram bits"
  | addr < 0x8000 = error "storeMBC: MBC1 rom/ram mode select"
  | otherwise = error "storeMBC: out of range"

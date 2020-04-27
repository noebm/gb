module Hardware.Cartridge.Bank.RomBank
  -- | General rom banks.
  -- | Supports swapping and generation from cartridge data.
  ( RomBankSelector (..)
  , defaultRomBankSelector

  , makeRomBanks
  , RomBanks

  , selectRomBank1
  , selectRomBank2
  , loadRom
  )
where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Control.Lens
import Control.Monad
import Data.Bits
import Data.Word

import Hardware.Cartridge.Rom

import Utilities.Vector

data RomBankSelector = RomBankSelector Int Int

defaultRomBankSelector :: RomBankSelector
defaultRomBankSelector = RomBankSelector 0 1

newtype RomBanks = RomBanks (V.Vector (VU.Vector Word8))

splitRomBanks :: VU.Vector Word8 -> Maybe (VU.Vector Word8, VU.Vector Word8)
splitRomBanks xs = do
  let (ys, zs) = VU.splitAt 0x4000 xs
  guard (VU.length ys /= 0)
  return (ys , zs)

makeRomBanks :: Rom -> RomBanks
makeRomBanks rom = RomBanks $ V.unfoldr splitRomBanks $ byteStringToVector (getRom rom)

selectRomBank1 :: Int -> (RomBanks -> Int)
selectRomBank1 i0 (RomBanks s) = i0 `mod` V.length s

selectRomBank2 :: Int -> (RomBanks -> Int)
selectRomBank2 i1 (RomBanks s) = i1 `mod` V.length s

-- could use MonadReader
loadRom :: RomBankSelector -> Word16 -> (RomBanks -> Word8)
loadRom (RomBankSelector i0 i1) addr (RomBanks s)
  | addr < 0x4000 = s ^?! ix i0 . ix (fromIntegral addr)
  | addr < 0x8000 = s ^?! ix i1 . ix (fromIntegral addr .&. 0x3fff)
  | otherwise = error "loadRom: index out of range"

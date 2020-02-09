module Cartridge.Bank.RomBank
  -- | General rom banks.
  -- | Supports swapping and generation from cartridge data.
  ( RomBank
  , defaultRomBank
  , makeRomBanks
  , selectRomBank
  , loadRom
  )
where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Control.Lens
import Control.Monad
import Data.Bits
import Data.Word

import Text.Printf

import Cartridge.Bank.Bank

data RomBank = RomBank Bank BankState

defaultRomBank :: RomBank
defaultRomBank = makeRomBanks (VU.replicate 0x8000 0x00)

splitRomBanks :: VU.Vector Word8 -> Maybe (VU.Vector Word8, VU.Vector Word8)
splitRomBanks xs = do
  let (ys, zs) = VU.splitAt 0x4000 xs
  guard (VU.length ys /= 0)
  when (VU.length ys /= 0x4000) $ error $ printf "splitRomBanks: invalid length %x" (VU.length ys)
  return (ys , zs)

makeRomBanks :: VU.Vector Word8 -> RomBank
makeRomBanks xs = do
  let vs = V.unfoldr splitRomBanks xs
  RomBank (vs V.! 0) (makeBanks 1 vs)

selectRomBank :: Int -> RomBank -> RomBank
selectRomBank i (RomBank s0 s) = RomBank s0 (swapBank i s)

loadRom :: RomBank -> Word16 -> Word8
loadRom (RomBank s0 s) addr
  | addr < 0x4000 = s0 VU.! fromIntegral addr
  | addr < 0x8000 = s ^?! activeBank . ix (fromIntegral addr .&. 0x3fff)
  | otherwise = error "loadRom: index out of range"

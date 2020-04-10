module Hardware.Cartridge.Bank.RamBank
  -- | General ram banks.
  -- | Supports swapping and initialization with zeros.
  ( RamBank
  , newRamBanks
  , selectRamBank
  , loadRam
  , storeRam
  )
where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Control.Lens
import Control.Monad
import Data.Bits
import Data.Word

import Hardware.Cartridge.Bank.Bank

newtype RamBank = RamBank BankState

newRamBanks :: Int -> Maybe RamBank
newRamBanks n = do
  guard (0 <= n)
  let vs = V.replicate n (VU.replicate 0x2000 0x00)
  return $ RamBank $ makeBanks 0 vs

selectRamBank :: Int -> RamBank -> RamBank
selectRamBank i (RamBank s) = RamBank (swapBank i s)

{-# INLINE inRamRange #-}
inRamRange :: (Num a, Ord a) => a -> Bool
inRamRange addr = 0xA000 <= addr && addr < 0xC000

loadRam :: RamBank -> Word16 -> Word8
loadRam (RamBank s) addr
  | inRamRange addr = s ^?! activeBank . ix (fromIntegral addr .&. 0x1fff)
  | otherwise = error "loadRam: index out of range"

storeRam :: Word16 -> Word8 -> RamBank -> RamBank
storeRam addr b (RamBank s)
  | inRamRange addr = s
    & activeBank . ix (fromIntegral addr .&. 0x1fff) .~ b
    & RamBank
  | otherwise = error "storeRam: index out of range"

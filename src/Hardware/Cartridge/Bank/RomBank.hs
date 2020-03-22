module Hardware.Cartridge.Bank.RomBank
  -- | General rom banks.
  -- | Supports swapping and generation from cartridge data.
  ( RomBank
  , defaultRomBank
  , makeRomBanks
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

import Text.Printf

import Hardware.Cartridge.Bank.Bank

data RomBank = RomBank Int Int Banks

defaultRomBank :: RomBank
defaultRomBank = makeRomBanks 2 (VU.replicate 0x8000 0x00)

splitRomBanks :: VU.Vector Word8 -> Maybe (VU.Vector Word8, VU.Vector Word8)
splitRomBanks xs = do
  let (ys, zs) = VU.splitAt 0x4000 xs
  guard (VU.length ys /= 0)
  when (VU.length ys /= 0x4000) $ error $ printf "splitRomBanks: invalid length %x" (VU.length ys)
  return (ys , zs)

makeRomBanks :: Word -> VU.Vector Word8 -> RomBank
makeRomBanks romBankCount xs =
  let vs = V.unfoldr splitRomBanks xs
  in
    if V.length vs /= fromIntegral romBankCount
    then error $ "makeRomBanks: should have " ++ show romBankCount ++ " banks, but got " ++ show (V.length vs)
    else RomBank 0 1 vs

selectRomBank1 :: Int -> RomBank -> RomBank
selectRomBank1 i0 (RomBank _ i1 s) = RomBank (i0 `mod` V.length s) i1 s

selectRomBank2 :: Int -> RomBank -> RomBank
selectRomBank2 i1 (RomBank i0 _ s) = RomBank i0 (i1 `mod` V.length s) s

loadRom :: RomBank -> Word16 -> Word8
loadRom (RomBank i0 i1 s) addr
  | addr < 0x4000 = s ^?! ix i0 . ix (fromIntegral addr)
  | addr < 0x8000 = s ^?! ix i1 . ix (fromIntegral addr .&. 0x3fff)
  | otherwise = error "loadRom: index out of range"

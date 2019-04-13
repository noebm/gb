{-# LANGUAGE TemplateHaskell #-}
module Cartridge.Controller
  -- | General rom banks.
  -- | Supports swapping and generation from cartridge data.
  ( RomBank
  , defaultRomBank
  , makeRomBanks
  , selectRomBank
  , loadRom

  -- | General ram banks.
  -- | Supports swapping and initialization with zeros.
  , RamBank
  , emptyRamBank
  , newRamBanks
  , selectRamBank
  , inRamRange
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
import Text.Printf

type Banks = V.Vector Bank

type Bank = VU.Vector Word8

data BankState = BankState
  { _banks :: Banks
  , _activeBankIndex :: Int
  , _activeBank :: VU.Vector Word8
  }

makeLenses ''BankState

-- makeBanks :: V.Vector Bank -> BankState s
makeBanks :: Int -> V.Vector Bank -> BankState
makeBanks i xs = BankState
  { _banks = xs
  , _activeBankIndex = i
  , _activeBank = xs ^. ix i
  }

putBank :: Bank -> Int -> Banks -> Banks
putBank v i s = s & ix i .~ v

swapBank :: Int -> BankState -> BankState
swapBank i' bs =
  let v  = bs ^. activeBank
      v' = bs ^. banks.ix i'
  in bs
    & banks %~ putBank v (bs ^. activeBankIndex)
    & activeBank .~ v'
    & activeBankIndex .~ i'

{-
  Rom bank code
-}
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
  | addr < 0x8000 = (s ^. activeBank) VU.! (fromIntegral addr .&. 0x3fff)
  | otherwise = error "loadRom: index out of range"

{-
  Ram bank code
-}
newtype RamBank = RamBank BankState

emptyRamBank :: RamBank
emptyRamBank = newRamBanks 0

newRamBanks :: Int -> RamBank
newRamBanks n =
  let vs = V.replicate n (VU.replicate 0x2000 0x00)
  in RamBank (makeBanks 0 vs)

selectRamBank :: Int -> RamBank -> RamBank
selectRamBank i (RamBank s) = RamBank (swapBank i s)

{-# INLINE inRamRange #-}
inRamRange :: (Num a, Ord a) => a -> Bool
inRamRange addr = 0xA000 <= addr && addr < 0xC000

loadRam :: RamBank -> Word16 -> Word8
loadRam (RamBank s) addr
  | inRamRange addr = (s ^. activeBank) VU.! (fromIntegral addr .&. 0x1fff)
  | otherwise = error "loadRam: index out of range"

storeRam :: Word16 -> Word8 -> RamBank -> RamBank
storeRam addr b (RamBank s)
  | inRamRange addr = s
    & activeBank .~ (s ^. activeBank) VU.// [ (fromIntegral addr .&. 0x1fff, b) ]
    & RamBank
  | otherwise = error "storeRam: index out of range"

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
import qualified Data.Vector.Unboxed.Mutable as VUM

import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Word

type Banks = V.Vector Bank

type Bank = VU.Vector Word8

data BankState s = BankState
  { _banks :: Banks
  , _activeBankIndex :: Int
  , _activeBank :: VUM.MVector s Word8
  }

makeLenses ''BankState

-- makeBanks :: V.Vector Bank -> BankState s
makeBanks :: PrimMonad m => Int -> V.Vector Bank -> m (BankState (PrimState m))
makeBanks i xs = do
  v <- VU.thaw $ xs ^. ix i
  return $ BankState
    { _banks = xs
    , _activeBankIndex = i
    , _activeBank = v
    }

putBank :: Bank -> Int -> Banks -> Banks
putBank v i s = s & ix i .~ v

getBank :: Int -> Banks -> Bank
getBank i s = s ^. ix i

swapBank :: PrimMonad m => Int -> BankState (PrimState m) -> m (BankState (PrimState m))
swapBank i' bs = do
  v  <- VU.freeze $ bs ^. activeBank
  v' <- VU.thaw $ bs ^. banks.ix i'
  return $ bs
    & banks %~ putBank v (bs ^. activeBankIndex)
    & activeBank .~ v'
    & activeBankIndex .~ i'

{-
  Rom bank code
-}
data RomBank s = RomBank Bank (BankState s)

defaultRomBank :: PrimMonad m => m (RomBank (PrimState m))
defaultRomBank = makeRomBanks (VU.replicate 0x8000 0x00)

splitRomBanks :: VU.Vector Word8 -> Maybe (VU.Vector Word8, VU.Vector Word8)
splitRomBanks xs = do
  let (ys, zs) = VU.splitAt 0x4000 xs
  when (VU.length ys /= 0x4000) $ error "splitRomBanks: invalid length"
  return (ys , zs)

makeRomBanks :: PrimMonad m => VU.Vector Word8 -> m (RomBank (PrimState m))
makeRomBanks xs = do
  let vs = V.unfoldr splitRomBanks xs
  RomBank (vs V.! 0) <$> makeBanks 1 vs

selectRomBank :: PrimMonad m => Int -> RomBank (PrimState m) -> m (RomBank (PrimState m))
selectRomBank i (RomBank s0 s) = do
  let i' = if i == 0 then 1 else i
  RomBank s0 <$> swapBank i' s

loadRom :: PrimMonad m => RomBank (PrimState m) -> Word16 -> m Word8
loadRom (RomBank s0 s) addr
  | addr < 0x4000 = return $ s0 VU.! fromIntegral addr
  | addr < 0x8000 = VUM.read (s ^. activeBank) (fromIntegral addr)
  | otherwise = error "loadRom: index out of range"

{-
  Ram bank code
-}
newtype RamBank s = RamBank (BankState s)

emptyRamBank :: PrimMonad m => m (RamBank (PrimState m))
emptyRamBank = newRamBanks 0

newRamBanks :: PrimMonad m => Int -> m (RamBank (PrimState m))
newRamBanks n = do
  let vs = V.replicate n (VU.replicate 0x2000 0x00)
  RamBank <$> makeBanks 0 vs

selectRamBank :: PrimMonad m => Int -> RamBank (PrimState m) -> m (RamBank (PrimState m))
selectRamBank i (RamBank s) = RamBank <$> swapBank i s

{-# INLINE inRamRange #-}
inRamRange :: (Num a, Ord a) => a -> Bool
inRamRange addr = 0xA000 <= addr && addr < 0xC000

loadRam :: PrimMonad m => RamBank (PrimState m) -> Word16 -> m Word8
loadRam (RamBank s) addr
  | inRamRange addr = VUM.read (s ^. activeBank) (fromIntegral addr .&. 0x1fff)
  | otherwise = error "loadRam: index out of range"

storeRam :: PrimMonad m => Word16 -> Word8 -> RamBank (PrimState m) -> m ()
storeRam addr b (RamBank s)
  | inRamRange addr = VUM.write (s ^. activeBank) (fromIntegral addr .&. 0x1fff) b
  | otherwise = error "storeRam: index out of range"

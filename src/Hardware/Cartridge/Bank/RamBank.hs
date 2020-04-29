{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Cartridge.Bank.RamBank
  -- | General ram banks.
  -- | Supports swapping and initialization with zeros.
  ( RamBank (..)
  , ramBankCount
  , ramBankBytes
  , newRamBanks
  , selectRamBank
  , loadRam
  , storeRam
  )
where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Data.Vector.Generic.Lens

import Control.Lens
import Control.Monad
import Data.Bits
import Data.Word

data RamBank = RamBank
  { _ramIndex :: !Int
  , _ramData :: V.Vector (VU.Vector Word8)
  }

makeLenses ''RamBank

{-# INLINE ramBankCount #-}
ramBankCount :: RamBank -> Int
ramBankCount = view (ramData . to V.length)

{-# INLINE ramBankBytes #-}
ramBankBytes :: IndexedTraversal' (Int, Int) RamBank Word8
ramBankBytes = ramData . vectorTraverse <.> vectorTraverse

{-# INLINE ram #-}
ram :: IndexedTraversal' Int RamBank (VU.Vector Word8)
ram f s = (ramData .> iix idx) f s
  where idx = s ^. ramIndex

newRamBanks :: Int -> Maybe RamBank
newRamBanks n = do
  guard (0 < n)
  let vs = V.replicate n (VU.replicate 0x2000 0x00)
  return $ RamBank 0 vs

selectRamBank :: Int -> RamBank -> RamBank
selectRamBank i s = s & ramIndex .~ (i `rem` ramBankCount s)

loadRam :: RamBank -> Word16 -> Word8
loadRam b addr = b ^?! ram . ix (fromIntegral addr .&. 0x1fff)

storeRam :: Word16 -> Word8 -> RamBank -> RamBank
storeRam addr dat = ram . ix (fromIntegral addr .&. 0x1fff) .~ dat

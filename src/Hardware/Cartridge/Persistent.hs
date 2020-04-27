{-# LANGUAGE DeriveGeneric #-}
module Hardware.Cartridge.Persistent where

import Data.Serialize
import GHC.Generics

import Hardware.Cartridge.Bank.RamBank
import qualified Hardware.Cartridge.Header as H

import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Utilities.Vector

import Data.Word

import Control.Lens
import Control.Monad

data CartridgeRAMSave = CartridgeRAMSave B.ByteString
  deriving Generic

instance Serialize CartridgeRAMSave

restoreRamBank :: CartridgeRAMSave -> RamBank
restoreRamBank (CartridgeRAMSave ram) = RamBank 0 $ V.unfoldr splitRamBanks $ byteStringToVector ram
  where
  splitRamBanks :: VU.Vector Word8 -> Maybe (VU.Vector Word8, VU.Vector Word8)
  splitRamBanks xs = do
    let (ys, zs) = VU.splitAt 0x2000 xs
    guard (VU.length ys /= 0)
    return (ys , zs)

saveRamBank :: RamBank -> CartridgeRAMSave
saveRamBank = CartridgeRAMSave . B.pack . toListOf ramBankBytes

saveAgreesWithHeader :: CartridgeRAMSave -> H.Header -> Bool
saveAgreesWithHeader save header = ramBankCount (restoreRamBank save) == fromIntegral (H.headerRamBanks header)

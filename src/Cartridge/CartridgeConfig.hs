module Cartridge.CartridgeConfig where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.ByteString as B

import Data.Bits
import Data.Word
import Control.Monad

import qualified Cartridge.Header as Header
import Cartridge.BootRom

data CartridgeConfig = CartridgeConfig
  { romBanks :: V.Vector RomBank
  , header :: Header.Header
  , bootRom :: Maybe BootRom
  }

newtype RomBank = RomBank B.ByteString

defaultRomBank :: RomBank
defaultRomBank = RomBank (B.replicate 0x4000 0x00)

readRomBank :: B.ByteString -> Maybe RomBank
readRomBank b = do
  guard (B.length b >= 0x4000)
  return $ RomBank $ B.take 0x4000 b

getRom :: Int -> RomBank -> Word8
getRom idx (RomBank b) = B.index b idx

data RamBankType
  = Ram2kb
  | Ram8kb
  | RamMBC2 -- 4 bit values x 512

data RamBank = RamBank
  { getRamBank :: VU.Vector Word8
  , ramBanktype :: RamBankType
  }

defaultRamBank :: RamBankType -> RamBank
defaultRamBank ty = RamBank (gen ty) ty where
  gen :: RamBankType -> VU.Vector Word8
  gen Ram2kb = VU.replicate 0x0800 0x00
  gen Ram8kb = VU.replicate 0x2000 0x00
  gen RamMBC2 = VU.replicate 512 0x00

getRam :: Int -> RamBank -> Word8
getRam idx (RamBank b ty) = accessor ty where
  accessor :: RamBankType -> Word8
  accessor RamMBC2 = b VU.! idx .&. 0xF
  accessor _       = b VU.! idx

setRam :: Int -> Word8 -> RamBank -> RamBank
setRam idx v (RamBank b ty) = RamBank (setter ty b) ty where
  setter :: RamBankType -> VU.Vector Word8 -> VU.Vector Word8
  setter RamMBC2 = VU.modify (\x -> VUM.write x idx $ 0xf .&. v)
  setter _       = VU.modify (\x -> VUM.write x idx v)



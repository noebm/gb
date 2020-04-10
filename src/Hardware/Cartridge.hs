module Hardware.Cartridge
  ( CartridgeState
  , makeCartridge

  , loadCartridge
  , storeCartridge

  , loadCartridgeRAM
  , storeCartridgeRAM

  , loadCartridgeBootRomRegister
  , storeCartridgeBootRomRegister

  , Rom (..)
  , readRom
  )
where

import Hardware.BootRom

import qualified Hardware.Cartridge.Header as Header
import Hardware.Cartridge.Bank.RomBank
import Hardware.Cartridge.Bank.RamBank
import Hardware.Cartridge.MemoryBankController
import Hardware.Cartridge.Rom

import Data.Word
import Data.Maybe

import Control.Monad.ST
import Data.STRef

data CartridgeState s = CartridgeState
  { header :: Header.Header
  , bootrom :: STRef s (Maybe BootRom)
  , romBanks :: RomBanks
  , mbc :: STRef s MemoryBankController
  }

makeCartridge :: Maybe BootRom -> Rom -> ST s (CartridgeState s)
makeCartridge boot rom = do
  let h = getRomHeader rom
  mbc' <- newSTRef $ newMemoryBankController h
  boot' <- newSTRef boot
  return $ CartridgeState
    { header = h
    , bootrom = boot'
    , romBanks = makeRomBanks rom
    , mbc = mbc'
    }

loadCartridge :: Word16 -> CartridgeState s -> ST s Word8
loadCartridge addr s
  | addr <= 0xff = do
      b <- readSTRef $ bootrom s
      romBankselect <- romBankSel <$> readSTRef (mbc s)
      return $
        maybe (loadRom romBankselect addr (romBanks s)) (loadBootRom addr) b
  | addr < 0x8000 = do
      romBankselect <- romBankSel <$> readSTRef (mbc s)
      return $ loadRom romBankselect addr (romBanks s)
  | otherwise = error "loadCartridge: address not in cartridge range"

loadCartridgeRAM :: Word16 -> CartridgeState s -> ST s Word8
loadCartridgeRAM addr s = do
  mbc' <- readSTRef (mbc s)
  return $ fromMaybe 0xff $ loadRam <$> ramBank mbc' <*> pure addr

storeCartridge :: Word16 -> Word8 -> CartridgeState s -> ST s ()
storeCartridge addr b c =
  modifySTRef (mbc c) (\m -> storeMBC addr b m (romBanks c))

storeCartridgeRAM :: Word16 -> Word8 -> CartridgeState s -> ST s ()
storeCartridgeRAM addr b c = modifySTRef (mbc c) $ \m ->
  maybe m (\mbcRAM -> m { mbcRamBank = Just mbcRAM }) $
  storeRam addr b <$> ramBank m

loadCartridgeBootRomRegister :: CartridgeState s -> ST s Bool
loadCartridgeBootRomRegister s = isNothing <$> readSTRef (bootrom s)

storeCartridgeBootRomRegister :: CartridgeState s -> ST s ()
storeCartridgeBootRomRegister c = writeSTRef (bootrom c) Nothing

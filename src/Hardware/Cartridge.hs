module Hardware.Cartridge
  ( CartridgeState
  , makeCartridge
  , CartridgeRAMSave
  , saveCartridge
  , loadCartridge
  , storeCartridge
  , loadCartridgeRAM
  , storeCartridgeRAM
  , loadCartridgeBootRomRegister
  , storeCartridgeBootRomRegister
  , Rom(..)
  , readRom
  ) where

import           Hardware.BootRom

import           Hardware.Cartridge.Bank.RamBank
import           Hardware.Cartridge.Bank.RomBank
import           Hardware.Cartridge.MemoryBankController
import           Hardware.Cartridge.Persistent
import           Hardware.Cartridge.Rom
import           Hardware.Cartridge.Rom.Header

import           Data.Maybe
import           Data.Word

import           Control.Monad.ST
import           Data.STRef

data CartridgeState s = CartridgeState
  { header   :: Header
  , bootrom  :: STRef s (Maybe BootRom)
  , romBanks :: RomBanks
  , mbc      :: STRef s MemoryBankController
  }

saveCartridge :: CartridgeState s -> ST s (Maybe CartridgeRAMSave)
saveCartridge = fmap (fmap saveRamBank . aux) . readSTRef . mbc where
  aux NoMemoryBankController        = Nothing
  aux (MemoryBankController1 _ _ x) = x

makeCartridge :: Maybe BootRom -> Rom -> ST s (CartridgeState s)
makeCartridge boot rom = do
  let h = getRomHeader rom
  mbc'  <- newSTRef $ newMemoryBankController h (persistent rom)
  boot' <- newSTRef boot
  return $ CartridgeState { header   = h
                          , bootrom  = boot'
                          , romBanks = makeRomBanks rom
                          , mbc      = mbc'
                          }

loadCartridge :: Word16 -> CartridgeState s -> ST s Word8
loadCartridge addr s
  | addr <= 0xff = do
    b             <- readSTRef $ bootrom s
    romBankselect <- romBankSel <$> readSTRef (mbc s)
    return
      $ maybe (loadRom romBankselect addr (romBanks s)) (loadBootRom addr) b
  | addr < 0x8000 = do
    romBankselect <- romBankSel <$> readSTRef (mbc s)
    return $ loadRom romBankselect addr (romBanks s)
  | otherwise = error "loadCartridge: address not in cartridge range"

loadCartridgeRAM :: Word16 -> CartridgeState s -> ST s Word8
loadCartridgeRAM addr s =
  maybe 0xff (`loadRam` addr) . ramBank <$> readSTRef (mbc s)

storeCartridge :: Word16 -> Word8 -> CartridgeState s -> ST s ()
storeCartridge addr b c =
  modifySTRef (mbc c) (\m -> storeMBC addr b m (romBanks c))

storeCartridgeRAM :: Word16 -> Word8 -> CartridgeState s -> ST s ()
storeCartridgeRAM addr b c = modifySTRef (mbc c) $ \m ->
  maybe m (\mbcRAM -> m { mbcRamBank = Just mbcRAM })
    $   storeRam addr b
    <$> ramBank m

loadCartridgeBootRomRegister :: CartridgeState s -> ST s Bool
loadCartridgeBootRomRegister s = isNothing <$> readSTRef (bootrom s)

storeCartridgeBootRomRegister :: CartridgeState s -> ST s ()
storeCartridgeBootRomRegister c = writeSTRef (bootrom c) Nothing

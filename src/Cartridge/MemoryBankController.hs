module Cartridge.MemoryBankController where

import Data.Bits
import Data.Word

import Cartridge.Bank.RamBank
import Cartridge.Bank.RomBank

import qualified Cartridge.Header as H

data MemoryBankController
  = NoMemoryBankController
  | MemoryBankController1 MBC1

defaultMBC :: MemoryBankController
defaultMBC = NoMemoryBankController

memoryBankController :: H.MBCType -> MemoryBankController
memoryBankController H.OnlyROM = NoMemoryBankController
memoryBankController H.MBC1    = MemoryBankController1 defaultMBC1

storeMBC :: Word16 -> Word8
         -> (MemoryBankController, RomBank, Maybe RamBank)
         -> (MemoryBankController, RomBank, Maybe RamBank)
storeMBC addr b (mbc, rom, ram) = case mbc of
  NoMemoryBankController -> (mbc, rom, ram)
  MemoryBankController1 mbc1 ->
    let mbc1' = storeMBC1 addr b mbc1
        (rom', ram') = updateBanks mbc1' (rom, ram)
    in (MemoryBankController1 mbc1', rom', ram')

ramAccessible :: MemoryBankController -> Bool
ramAccessible NoMemoryBankController = False
ramAccessible (MemoryBankController1 mbc1) = ramg mbc1

data MBC1Mode = MBC1_RomMode | MBC1_RamMode
  deriving Eq

data MBC1 = MBC1
  { ramg :: Bool
  , bank1 :: Word8
  , bank2 :: Word8
  , mode :: MBC1Mode
  }

defaultMBC1 :: MBC1
defaultMBC1 = MBC1 { ramg = False , bank1 = 0x01, bank2 = 0x00, mode = MBC1_RomMode }

romBank1Index :: MBC1 -> Word8
romBank1Index s = case mode s of
  MBC1_RamMode -> (bank2 s .&. 0x03) `shiftL` 5
  MBC1_RomMode -> 0x00

romBank2Index :: MBC1 -> Word8
romBank2Index s =
  let lo = bank1 s .&. 0x1f
      hi = (bank2 s .&. 0x03) `shiftL` 5
  in hi .|. lo

ramBankIndex :: MBC1 -> Word8
ramBankIndex s = case mode s of
  MBC1_RamMode -> bank2 s .&. 0x03
  MBC1_RomMode -> 0x00

updateBanks :: MBC1 -> (RomBank, Maybe RamBank) -> (RomBank, Maybe RamBank) -- CartridgeState -> CartridgeState
updateBanks mbc1 (romBanks , ramBanks) =
  ( selectRomBank1 (fromIntegral $ romBank1Index $ mbc1) $
    selectRomBank2 (fromIntegral $ romBank2Index $ mbc1) $
    romBanks
  , selectRamBank (fromIntegral $ ramBankIndex $ mbc1) <$> ramBanks
  )

storeMBC1 :: Word16 -> Word8 -> MBC1 -> MBC1
storeMBC1 addr b s
  | addr < 0x2000 = s { ramg = b .&. 0xF == 0xA }
  | addr < 0x4000 = s { bank1 = let x = b .&. 0x1f in if x == 0 then 1 else x }
  | addr < 0x6000 = s { bank2 = b .&. 0x03 }
  | addr < 0x8000 = s { mode = if b `testBit` 0 then MBC1_RamMode else MBC1_RomMode }
  | otherwise = error "storeMBC: out of range"

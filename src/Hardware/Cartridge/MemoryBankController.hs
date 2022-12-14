{-# LANGUAGE RecordWildCards #-}
module Hardware.Cartridge.MemoryBankController where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Word

import           Hardware.Cartridge.Bank.RamBank
import           Hardware.Cartridge.Bank.RomBank
import           Hardware.Cartridge.Persistent

import           Hardware.Cartridge.Rom.Header

data MemoryBankController
  = NoMemoryBankController
  | MemoryBankController1
    { mbcConfig :: MBC1
    , mbcRomSelector :: RomBankSelector
    , mbcRamBank :: Maybe RamBank
    }

newMemoryBankController
  :: Header -> Maybe CartridgeRAMSave -> MemoryBankController
newMemoryBankController h s = case headerType h of
  HasNoMBC _  -> NoMemoryBankController
  HasMBC1 _ f -> MemoryBankController1
    (defaultMBC1 f)
    defaultRomBankSelector
    ((restoreRamBank <$> s) <|> newRamBanks (fromIntegral $ headerRamBanks h))

storeMBC
  :: Word16 -> Word8 -> MemoryBankController -> RomBanks -> MemoryBankController
storeMBC addr b mbc banks = case mbc of
  NoMemoryBankController -> NoMemoryBankController
  MemoryBankController1 mbc1 _ ram ->
    let mbc1'        = storeMBC1 addr b mbc1
        (rom', ram') = updateBanks mbc1' ram banks
    in  MemoryBankController1 mbc1' rom' ram'

romBankSel :: MemoryBankController -> RomBankSelector
romBankSel NoMemoryBankController          = RomBankSelector 0 1
romBankSel (MemoryBankController1 _ sel _) = sel

ramBank :: MemoryBankController -> Maybe RamBank
ramBank NoMemoryBankController              = Nothing
ramBank (MemoryBankController1 mbc1 _ ramb) = do
  guard (ramg mbc1)
  ramb

data MBC1Mode = MBC1_RomMode | MBC1_RamMode
  deriving (Show, Eq)

data MBC1 = MBC1
  { ramg      :: Bool
  , bank1     :: Word8
  , bank2     :: Word8
  , mode      :: MBC1Mode
  , multiCart :: Bool
  }
  deriving Show

defaultMBC1 :: Bool -> MBC1
defaultMBC1 multirom = MBC1 { ramg      = False
                            , bank1     = 0x01
                            , bank2     = 0x00
                            , mode      = MBC1_RomMode
                            , multiCart = multirom
                            }

bank1Mask :: Bool -> Word8
bank1Mask multiCart = if multiCart then 0x0f else 0x1f

bank2Shift :: Bool -> Int
bank2Shift multiCart = if multiCart then 4 else 5

romBank1Index :: MBC1 -> Word8
romBank1Index MBC1 {..} = case mode of
  MBC1_RamMode -> (bank2 .&. 0x03) `shiftL` bank2Shift multiCart
  MBC1_RomMode -> 0x00

romBank2Index :: MBC1 -> Word8
romBank2Index MBC1 {..} =
  let lo = bank1 .&. bank1Mask multiCart
      hi = (bank2 .&. 0x03) `shiftL` bank2Shift multiCart
  in  hi .|. lo

ramBankIndex :: MBC1 -> Word8
ramBankIndex MBC1 {..} = case mode of
  MBC1_RamMode -> bank2 .&. 0x03
  MBC1_RomMode -> 0x00

generateRomBankSelector :: MBC1 -> (RomBanks -> RomBankSelector)
generateRomBankSelector mbc1 banks = RomBankSelector
  (selectRomBank1 (fromIntegral $ romBank1Index mbc1) banks)
  (selectRomBank2 (fromIntegral $ romBank2Index mbc1) banks)

updateBanks
  :: MBC1 -> Maybe RamBank -> (RomBanks -> (RomBankSelector, Maybe RamBank))
updateBanks mbc1 ramBanks banks =
  ( generateRomBankSelector mbc1 banks
  , selectRamBank (fromIntegral $ ramBankIndex mbc1) <$> ramBanks
  )

storeMBC1 :: Word16 -> Word8 -> MBC1 -> MBC1
storeMBC1 addr b s
  | addr < 0x2000 = s { ramg = b .&. 0xF == 0xA }
  | addr < 0x4000 = s { bank1 = let x = b .&. 0x1f in if x == 0 then 1 else x }
  | addr < 0x6000 = s { bank2 = b .&. 0x03 }
  | addr < 0x8000 = s
    { mode = if b `testBit` 0 then MBC1_RamMode else MBC1_RomMode
    }
  | otherwise = error "storeMBC: out of range"

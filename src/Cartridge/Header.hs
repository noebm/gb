{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Cartridge.Header
  ( Header(..)
  , header

  , MBCType (..)
  , _HasRAM, _IsPersistent
  , mbcType

  , CartridgeOptions (..)
  , CartridgeType (..)
  )
where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word

import Control.Lens
import Control.Monad

data CartridgeOptions
  = HasRAM
  | IsPersistent
  deriving Show

makePrisms ''CartridgeOptions

data MBCType
  = OnlyROM
  | MBC1
  | MBC2
  deriving Show

data CartridgeType = CartridgeType
  { _mbcType :: MBCType
  , _cartridgeOptions :: [ CartridgeOptions ]
  }
  deriving Show

makeLenses ''CartridgeType

data Header = Header
  { headerTitle    :: ByteString
  , headerType     :: CartridgeType
  , headerRomBanks :: Word
  , headerRamBanks :: Word
  , headerLocale   :: Word8
  }
  deriving Show

header :: ByteString -> Maybe Header
header bs = do
  guard $ B.length bs >= 0x150
  let (crc, crc') = checksumHeader bs
  guard (crc == crc')
  return $ Header
    { headerTitle = title bs
    , headerType = cartridgeType (bs `B.index` 0x147)
    , headerRomBanks = romBanks bs
    , headerRamBanks = ramBanks bs
    , headerLocale = locale bs
    }

checksumHeader :: ByteString -> (Word8 , Word8)
checksumHeader bs = (foldl (\x y -> x - y - 1) 0 checksumData, headerChecksum)
  where
    headerChecksum = bs `B.index` 0x14D
    checksumData = B.unpack $ B.take (0x14D - 0x134) $ B.drop 0x134 bs

title :: ByteString -> ByteString
title = B.take 16 . B.takeWhile (/= 0) . B.drop 0x134

cartridgeType :: Word8 -> CartridgeType
cartridgeType 0 = CartridgeType OnlyROM []
cartridgeType 8 = CartridgeType OnlyROM [ HasRAM ]
cartridgeType 9 = CartridgeType OnlyROM [ HasRAM, IsPersistent ]

cartridgeType 1 = CartridgeType MBC1 []
cartridgeType 2 = CartridgeType MBC1 [ HasRAM ]
cartridgeType 3 = CartridgeType MBC1 [ HasRAM, IsPersistent ]

cartridgeType 5 = CartridgeType MBC2 []
cartridgeType 6 = CartridgeType MBC2 [ IsPersistent ]

romBanks :: ByteString -> Word
romBanks bs = 4 * fromIntegral (bs `B.index` 0x148)

locale :: ByteString -> Word8
locale bs = bs `B.index` 0x14A

ramBanks :: ByteString -> Word
ramBanks bs = case bs `B.index` 0x149 of
  0x00 -> 0
  0x01 -> 2
  0x02 -> 8
  0x03 -> 32
  _ -> error "RAM size not defined"

-- cartridgeTypeSupported :: Word8 -> Bool
-- cartridgeTypeSupported 0x00 = True
-- cartridgeTypeSupported 0x01 = True
-- cartridgeTypeSupported _ = False



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
import Data.Bits

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
--   | MBC2
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

header :: ByteString -> Either String Header
header bs = do
  unless (B.length bs >= 0x150) $ Left "header: file too short"
  let (crc, crc') = checksumHeader bs
  unless (crc == crc') $ Left "header: invalid checksum"
  ram <- ramBanks bs
  loc <- locale bs
  cartTy <- cartridgeType (bs `B.index` 0x147)
  return $ Header
    { headerTitle = title bs
    , headerType = cartTy
    , headerRomBanks = romBanks bs
    , headerRamBanks = ram
    , headerLocale = loc
    }

checksumHeader :: ByteString -> (Word8 , Word8)
checksumHeader bs = (foldl (\x y -> x - y - 1) 0 checksumData, headerChecksum)
  where
    headerChecksum = bs `B.index` 0x14D
    checksumData = B.unpack $ B.take (0x14D - 0x134) $ B.drop 0x134 bs

title :: ByteString -> ByteString
title = B.take 16 . B.takeWhile (/= 0) . B.drop 0x134

cartridgeType :: Word8 -> Either String CartridgeType
cartridgeType 0 = Right $ CartridgeType OnlyROM []
cartridgeType 8 = Right $ CartridgeType OnlyROM [ HasRAM ]
cartridgeType 9 = Right $ CartridgeType OnlyROM [ HasRAM, IsPersistent ]

cartridgeType 1 = Right $ CartridgeType MBC1 []
cartridgeType 2 = Right $ CartridgeType MBC1 [ HasRAM ]
cartridgeType 3 = Right $ CartridgeType MBC1 [ HasRAM, IsPersistent ]

-- cartridgeType 5 = Right $ CartridgeType MBC2 []
-- cartridgeType 6 = Right $ CartridgeType MBC2 [ IsPersistent ]
cartridgeType x = Left $ "cartridgetype invalid / not supported " ++ show x

romBanks :: ByteString -> Word
romBanks bs = case bs `B.index` 0x148 of
  x -> 2 `shiftL` fromIntegral x

locale :: ByteString -> Either String Word8
locale bs = Right $ bs `B.index` 0x14A

ramBanks :: ByteString -> Either String Word
ramBanks bs = case bs `B.index` 0x149 of
  0x00 -> Right 0
  0x01 -> Right 2
  0x02 -> Right 8
  0x03 -> Right 32
  _ -> Left "RAM size not defined"

-- cartridgeTypeSupported :: Word8 -> Bool
-- cartridgeTypeSupported 0x00 = True
-- cartridgeTypeSupported 0x01 = True
-- cartridgeTypeSupported _ = False



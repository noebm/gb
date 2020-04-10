{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Hardware.Cartridge.Header
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

import Data.Serialize.Get

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
header = runGet $ do
  skip 0x100
  isolate 0x50 $ do
    label "program entry point" $ skip 4
    label "nintendo logo" $ skip 0x30

    header' <- lookAhead $ do
      -- either 16 byte title or title + manufacturer code + cgb flag
      title <- label "title" $ B.takeWhile (/= 0x00) <$> getBytes 0x10
      -- label "manufacturer code" $ skip 4
      -- _cbg <- label "cgb flag" $ getWord8
      _code <- label "new licensee code" $ getBytes 2
      _sgb <- label "sgb flag" getWord8
      cartTy <- either fail return . cartridgeType =<< label "cartridge type" getWord8
      rom <- shiftL 2 . fromIntegral <$> label "ROM size" getWord8
      ram <- either fail return . ramBanks =<< label "RAM size" getWord8
      loc <- label "locale" getWord8
      label "old licensee code" $ skip 1
      label "game version number" $ skip 1
      return $ Header
        { headerTitle = title
        , headerType  = cartTy
        , headerRomBanks = rom
        , headerRamBanks = ram
        , headerLocale   = loc
        }

    headerchksm' <- B.foldl (\x y -> x - y - 1) 0x00 <$> getBytes 0x19
    headerchksm <- label "header checksum" getWord8
    -- checksum over whole ROM (excluding both checksum bytes)
    _globalchksm <- label "global checksum" getWord16be

    unless (headerchksm == headerchksm') $ fail "checksum failed"
    return header'

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

ramBanks :: Word8 -> Either String Word
ramBanks x = case x of
  0x00 -> Right 0
  0x01 -> Right 2
  0x02 -> Right 8
  0x03 -> Right 32
  _ -> Left "RAM size not defined"

-- cartridgeTypeSupported :: Word8 -> Bool
-- cartridgeTypeSupported 0x00 = True
-- cartridgeTypeSupported 0x01 = True
-- cartridgeTypeSupported _ = False



{-# LANGUAGE TemplateHaskell #-}
module Hardware.Cartridge.Rom.Header
  ( Header(..)
  , parseHeader

  , CartridgeType (..)
  , _HasNoMBC, _HasMBC1
  , cartridgeMemoryType

  , CartridgeMemoryType (..)
  , _MemoryWithoutBattery, _MemoryWithBattery
  )
where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word
import Data.Bits

import Control.Lens
import Control.Monad

import Data.Serialize.Get

data CartridgeMemoryType = MemoryWithoutBattery | MemoryWithBattery
  deriving Show

makePrisms ''CartridgeMemoryType

data CartridgeType
  = HasNoMBC (Maybe CartridgeMemoryType)
  | HasMBC1  (Maybe CartridgeMemoryType)
  deriving Show

makePrisms ''CartridgeType

cartridgeMemoryType :: Traversal' CartridgeType CartridgeMemoryType
cartridgeMemoryType f (HasNoMBC x) = HasNoMBC <$> _Just f x
cartridgeMemoryType f (HasMBC1  x) = HasMBC1  <$> _Just f x

data Header = Header
  { headerTitle    :: ByteString
  , headerType     :: CartridgeType
  , headerRomBanks :: Word
  , headerRamBanks :: Word
  , headerLocale   :: Word8
  }
  deriving Show

calculateChecksum :: ByteString -> Word8
calculateChecksum = B.foldl (\x y -> x - y - 1) 0x00

parseHeader :: ByteString -> Either String Header
parseHeader = runGet $ do
  skip 0x100
  isolate 0x50 $ do
    label "program entry point" $ skip 4
    label "nintendo logo" $ skip 0x30

    -- 0x14b should be
    newLicenseeCode <- lookAhead $ skip 0x16 *> ((0x33 ==) <$> getWord8)

    header' <- lookAhead $ do
      -- either 16 byte title or title + manufacturer code + cgb flag
      let titleSize = if newLicenseeCode then 0x0b else 0x10
      title <- label "title" $ B.takeWhile (/= 0x00) <$> getBytes titleSize
      -- dont parse manufacturer code for now
      skip $ 0x10 - titleSize
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

    headerchksm' <- calculateChecksum <$> getBytes 0x19
    headerchksm <- label "header checksum" getWord8
    -- checksum over whole ROM (excluding both checksum bytes)
    _globalchksm <- label "global checksum" getWord16be

    unless (headerchksm == headerchksm') $ fail "checksum failed"
    return header'

cartridgeType :: Word8 -> Either String CartridgeType
cartridgeType 0 = Right $ HasNoMBC Nothing
cartridgeType 8 = Right $ HasNoMBC (Just MemoryWithoutBattery)
cartridgeType 9 = Right $ HasNoMBC (Just MemoryWithBattery)

cartridgeType 1 = Right $ HasMBC1 Nothing
cartridgeType 2 = Right $ HasMBC1 (Just MemoryWithoutBattery)
cartridgeType 3 = Right $ HasMBC1 (Just MemoryWithBattery)

cartridgeType x = Left $ "cartridgetype invalid / not supported " ++ show x

ramBanks :: Word8 -> Either String Word
ramBanks x = case x of
  0x00 -> Right 0
  -- idk since it is smaller than 0x2000 = 8kB
  -- so only a partial ram bank of size 0x800
  -- 0x01 -> Right 2
  0x02 -> Right 1
  0x03 -> Right 4
  _ -> Left "RAM size not defined"

-- cartridgeTypeSupported :: Word8 -> Bool
-- cartridgeTypeSupported 0x00 = True
-- cartridgeTypeSupported 0x01 = True
-- cartridgeTypeSupported _ = False



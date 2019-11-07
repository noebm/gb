{-# LANGUAGE TemplateHaskell #-}
module Cartridge.CartridgeConfig where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.ByteString as B

import Data.Bits
import Data.Word
import Data.Maybe

import Control.Monad
import Control.Lens

import Data.Attoparsec.ByteString as Parse

-- import qualified Cartridge.Header as Header
import Cartridge.BootRom

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

cartridgeType' :: Word8 -> CartridgeType
cartridgeType' 0x00 = CartridgeType OnlyROM []
cartridgeType' 0x08 = CartridgeType OnlyROM [ HasRAM ]
cartridgeType' 0x09 = CartridgeType OnlyROM [ HasRAM, IsPersistent ]

cartridgeType' 0x01 = CartridgeType MBC1 []
cartridgeType' 0x02 = CartridgeType MBC1 [ HasRAM ]
cartridgeType' 0x03 = CartridgeType MBC1 [ HasRAM, IsPersistent ]

cartridgeType' 0x05 = CartridgeType MBC2 []
cartridgeType' 0x06 = CartridgeType MBC2 [ IsPersistent ]

data CartridgeConfig = CartridgeConfig
  -- { title :: B.ByteString
  -- , manufacturerCode :: Maybe B.ByteString
  -- , cgbFlag :: Maybe CGBFlag
  -- , licenseeCode :: Word8
  -- , sgbFlag :: Bool
  -- , cartridgeType :: CartridgeType
    -- header :: Header.Header
  { cartridgeHeader :: Header
  , romBanks :: V.Vector RomBank
  , bootRom :: Maybe BootRom
  }



parseTitleField :: Parser (B.ByteString, Maybe B.ByteString, Maybe CGBFlag)
parseTitleField = do
  b <- Parse.takeWhile (/= 0)
  let l = B.length b

  manu <- if l <= 11
    then Parse.take (11 - l) *> (Just <$> Parse.take 4)
    else return Nothing

  unless (isJust manu) $ void $ Parse.take $ 15 - l

  cgb <- if l <= 15
    then cgbFlag' <$> anyWord8
    else return Nothing

  guard (l <= 16)
  return (b, manu, cgb)

data CGBFlag
  = CGBSupported
  | CGBMandatory

cgbFlag' :: Word8 -> Maybe CGBFlag
cgbFlag' 0x80 = Just CGBSupported
cgbFlag' 0xC0 = Just CGBMandatory
cgbFlag' _ = Nothing

data SGBFlag
  = SGBSupport
  | SGBUnsupported
  | SGBDisabled

sgbFlag' :: Word8 -> SGBFlag
sgbFlag' 0x30 = SGBSupport
sgbFlag' 0x00 = SGBUnsupported
sgbFlag' _    = SGBDisabled

data RAMSize = RAMSize8KB Word | RAMSize2KB

data Header = Header
  { title :: B.ByteString
  , manufacturerCode :: Maybe B.ByteString
  , cgbFlag :: Maybe CGBFlag
  , sgbFlag :: SGBFlag
  , cartridgeType :: CartridgeType
  , romBankNumber :: Word
  , ramSize :: Maybe RAMSize
  -- , licenseeCode :: Word8
  , version :: Word8
  }

parseHeader :: Parser Header
parseHeader = do
  _ <- Parse.take 0x100 -- part before header
  _entryPoint <- Parse.take 4
  _logo <- Parse.take 48

  (bs , cartHeader) <- match $ do
    (title', manufacturer, cgb) <- parseTitleField
    _newLicensee <- Parse.take 2
    sgb   <- sgbFlag' <$> anyWord8
    ctype <- cartridgeType' <$> anyWord8
    rombanks <- (4 *) . fromIntegral <$> anyWord8
    ramsize <- flip (<$>) anyWord8 $ \x -> case x of
      0x00 -> Nothing
      0x01 -> Just RAMSize2KB
      0x02 -> Just $ RAMSize8KB 1
      0x03 -> Just $ RAMSize8KB 4
      0x04 -> Just $ RAMSize8KB 16
      0x05 -> Just $ RAMSize8KB 8
      _ -> fail "invalid ram size"
    _destCode <- anyWord8
    _oldLicensee <- anyWord8
    version' <- anyWord8

    return $ Header title' manufacturer cgb sgb ctype rombanks ramsize version'

  let checksumCalculated = B.foldl (\acc x -> acc - x - 1) 0x00 bs
  checksumExpected <- anyWord8

  when (checksumCalculated /= checksumExpected) $ fail "checksum failed"
  return cartHeader

-- parseCartridgeConfig :: Parser CartridgeConfig
-- parseCartridgeConfig = do
--   (bs0, hd) <- match parseHeader
-- 
--   return $ 

readCartridgeConfig :: B.ByteString -> Maybe BootRom -> Either String CartridgeConfig
readCartridgeConfig b brom =
  let h = parseOnly parseHeader b
      roms = V.unfoldr readRomBank b
  in CartridgeConfig <$> h <*> pure roms <*> pure brom

newtype RomBank = RomBank B.ByteString

defaultRomBank :: RomBank
defaultRomBank = RomBank (B.replicate 0x4000 0x00)

-- readRomBanks :: B.ByteString -> omBank

readRomBank :: B.ByteString -> Maybe (RomBank , B.ByteString)
readRomBank b = do
  guard (B.length b >= 0x4000)
  let (c , d) = B.splitAt 0x4000 b
  return (RomBank c , d)

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



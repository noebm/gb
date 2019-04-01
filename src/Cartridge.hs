module Cartridge
where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word
import Text.Printf

import Control.Monad

data Cartridge = Cartridge
  { cartridgeData     :: ByteString
  , cartridgeTitle    :: ByteString
  , cartridgeType     :: Word8
  , cartridgeRomBanks :: Word
  , cartridgeRamBanks :: Word
  , cartridgeLocale   :: Word8
  }

instance Show Cartridge where
  show c = "Cartridge " ++ show (cartridgeTitle c)

checksumHeader :: ByteString -> (Word8 , Word8)
checksumHeader bs = (foldl (\x y -> x - y - 1) 0 checksumData, headerChecksum)
  where
    headerChecksum = bs `B.index` 0x14D
    checksumData = B.unpack $ B.take (0x14D - 0x134) $ B.drop 0x134 bs

headerTitle :: ByteString -> ByteString
headerTitle = B.take 16 . B.takeWhile (/= 0) . B.drop 0x134

headerType :: ByteString -> Word8
headerType bs = bs `B.index` 0x147

headerRomBanks :: ByteString -> Word
headerRomBanks bs = 4 * fromIntegral (bs `B.index` 0x148)

headerLocale :: ByteString -> Word8
headerLocale bs = bs `B.index` 0x14A

headerRamBanks :: ByteString -> Either String Word
headerRamBanks bs = case bs `B.index` 0x149 of
  0x00 -> Right 0
  0x01 -> Right 2
  0x02 -> Right 8
  0x03 -> Right 32
  _ -> Left "RAM size not defined"

loadCartridge :: FilePath -> IO (Either String Cartridge)
loadCartridge fp = do
  f <- B.readFile fp
  return $ do
    when (B.length f < 0x150) $ Left "File too short to contain a header"
    let (checksumResult, checksum) = checksumHeader f
    when (checksumResult /= checksum) $ Left "Header checksum does not match"
    when (headerType f /= 0x00) $ Left $ printf "Cartridge type (0x%02x) unsupported" (headerType f)
    Cartridge f (headerTitle f) (headerType f) (headerRomBanks f) <$> headerRamBanks f <*> pure (headerLocale f)

getRomBank :: Cartridge -> Word -> Maybe ByteString
getRomBank c idx = do
  let banksize = 0x4000
  guard (idx < cartridgeRomBanks c || idx < 2)
  return $ B.take banksize $ B.drop (fromIntegral (idx - 1) * banksize) (cartridgeData c)

memoryBootRom :: IO ByteString
memoryBootRom = do
  let bootStrapName = "DMG_ROM.bin"
  B.readFile $ "./" ++ bootStrapName

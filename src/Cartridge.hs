module Cartridge
where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word

import Control.Monad

data Cartridge = Cartridge
  { cartridgeData :: ByteString
  , cartridgeTitle :: ByteString
  , cartridgeType :: Word8
  , cartridgeRomBanks :: Word
  , cartridgeRamSize :: Word
  , cartridgeLocale :: Word8
  }

instance Show Cartridge where
  show c = "Cartridge " ++ show (cartridgeTitle c)

checksumHeader :: ByteString -> Maybe (Word8 , Word8)
checksumHeader bs
  | B.length bs >= 0x150 = Just (foldl (\x y -> x - y - 1) 0 checksumData, headerChecksum)
  | otherwise = Nothing
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

headerRamSize :: ByteString -> Word
headerRamSize bs = case bs `B.index` 0x149 of
  0x00 -> 0
  0x01 -> 2
  0x02 -> 8
  0x03 -> 32
  _ -> error "help ram size not defined"

loadCartridge :: FilePath -> IO (Maybe Cartridge)
loadCartridge fp = do
  f <- B.readFile fp
  return $ do
    (checksumResult, checksum) <- checksumHeader f
    guard (checksumResult == checksum)
    return $ Cartridge f (headerTitle f) (headerType f) (headerRomBanks f) (headerRamSize f) (headerLocale f)

getRomBank :: Cartridge -> Word -> Maybe ByteString
getRomBank c idx = do
  let banksize = 0x4000
  guard (idx < cartridgeRomBanks c || idx < 2)
  return $ B.take banksize $ B.drop (fromIntegral (idx - 1) * banksize) (cartridgeData c)

memoryBootRom :: IO ByteString
memoryBootRom = do
  let bootStrapName = "DMG_ROM.bin"
  B.readFile $ "./" ++ bootStrapName

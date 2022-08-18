module Hardware.Cartridge.Rom
  ( Rom(..)
  , romSaveFilePath
  , readRom
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except

import           Hardware.Cartridge.Persistent
import           Hardware.Cartridge.Rom.Header

import qualified Data.ByteString               as B

import           Data.Serialize

import           System.Directory
import           System.FilePath
import           Text.Printf

import           Data.Digest.CRC32
import           Data.Functor
import           Data.List

data Rom = Rom
  { romFilePath  :: FilePath
  , getRomHeader :: Header
  , getRom       :: B.ByteString
  , persistent   :: Maybe CartridgeRAMSave
  }

romSaveFilePath :: Rom -> FilePath
romSaveFilePath rom = addExtension (romFilePath rom) ".save"

tryLoadSaveFile :: Rom -> ExceptT String IO (Maybe CartridgeRAMSave)
tryLoadSaveFile rom = do
  -- check for ram file
  let ramfp = romSaveFilePath rom
  hasRamFile <- liftIO $ doesFileExist ramfp

  saveFile   <- forM (ramfp <$ guard hasRamFile) $ \fp -> do
    liftIO $ putStrLn $ "loading save file from " ++ fp
    liftEither . runGet get <=< liftIO . B.readFile $ fp

  forM_ saveFile $ \dat -> unless (saveAgreesWithHeader dat (getRomHeader rom))
    $ throwError "save file does not agree with the rom header"

  return saveFile

readRom' :: FilePath -> ExceptT String IO Rom
readRom' fp = do
  bytes <- liftIO $ B.readFile fp
  -- sanity checks
  let (q, r) = B.length bytes `quotRem` 0x4000
  -- at least 0x8000 bytes long
  when (q < 2) $ throwError "rom file shorter than 0x8000 bytes"
  -- a multiple of 0x4000 bytes long
  when (r /= 0) $ throwError $ printf
    "file length (0x%x) not a multiple of 0x4000 bytes"
    (B.length bytes)

  -- verify that the header agrees with the rom data
  h <- liftEither $ parseHeader bytes
  let romBankCount = fromIntegral $ headerRomBanks h
  when (q /= romBankCount) $ throwError $ printf
    "number of banks does not match - should have %i but got %i"
    romBankCount
    q

  let h' = h
        { headerType = headerType h
                       &  cartridgeMBC1MultiCart
                       .~ isMultiCart bytes
        }

  return $ Rom fp h' bytes Nothing

-- | Check for mbc1 multicarts
isMultiCart :: B.ByteString -> Bool
isMultiCart romBytes =
  B.length romBytes
    == 2
    ^  20 -- only known multicart size
    && (length headers >= 3)
 where
  headers =
    filter ((== 0x46195417) . crc32 . B.take 0x30 . B.drop 0x104) $ unfoldr
      (\bytes ->
        let (xs, ys) = B.splitAt 0x40000 bytes
        in  guard (not $ B.null ys) $> (xs, ys)
      )
      romBytes

readRom :: Bool -> FilePath -> ExceptT String IO Rom
readRom loadSaves fp = do
  rom' <- readRom' fp

  let
    cartPersistent =
      has (cartridgeMemoryType . _MemoryWithBattery) $ headerType $ getRomHeader
        rom'

  if loadSaves && cartPersistent
    then do
      saveFile <- tryLoadSaveFile rom'
      return $ rom' { persistent = saveFile }
    else return rom'

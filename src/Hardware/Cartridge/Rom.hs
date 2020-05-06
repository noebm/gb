module Hardware.Cartridge.Rom
  ( Rom (..)
  , romSaveFilePath
  , readRom
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.Except

import Hardware.Cartridge.Rom.Header
import Hardware.Cartridge.Persistent

import qualified Data.ByteString as B

import Data.Serialize

import Text.Printf
import System.FilePath
import System.Directory

data Rom = Rom
  { romFilePath :: FilePath
  , getRomHeader :: Header
  , getRom :: B.ByteString
  , persistent :: Maybe CartridgeRAMSave
  }

romSaveFilePath :: Rom -> FilePath
romSaveFilePath rom = addExtension (romFilePath rom) ".save"

tryLoadSaveFile :: Rom -> ExceptT String IO (Maybe CartridgeRAMSave)
tryLoadSaveFile rom = do
  -- check for ram file
  let ramfp = romSaveFilePath rom
  hasRamFile <- liftIO $ doesFileExist ramfp

  saveFile <- forM (ramfp <$ guard hasRamFile) $ \fp -> do
    liftIO $ putStrLn $ "loading save file from " ++ fp
    liftEither . runGet get <=< liftIO . B.readFile $ fp

  forM_ saveFile $ \dat ->
    unless (saveAgreesWithHeader dat (getRomHeader rom))
    $ throwError "save file does not agree with the rom header"

  return saveFile

readRom' :: FilePath -> ExceptT String IO Rom
readRom' fp = do
  bytes <- liftIO $ B.readFile fp
  -- sanity checks
  let (q , r) = B.length bytes `quotRem` 0x4000
  -- at least 0x8000 bytes long
  when (q < 2) $ throwError "rom file shorter than 0x8000 bytes"
  -- a multiple of 0x4000 bytes long
  when (r /= 0) $ throwError
    $ printf "file length (0x%x) not a multiple of 0x4000 bytes" (B.length bytes)

  -- verify that the header agrees with the rom data
  h <- liftEither $ parseHeader bytes
  let romBankCount = fromIntegral $ headerRomBanks h
  when (q /= romBankCount)
    $ throwError
    $ printf "number of banks does not match - should have %i but got %i"
    romBankCount q

  return $ Rom fp h bytes Nothing

readRom :: FilePath -> ExceptT String IO Rom
readRom fp = do
  rom' <- readRom' fp

  let cartPersistent
        = has (cartridgeMemoryType . _MemoryWithBattery)
        $ headerType $ getRomHeader rom'

  if cartPersistent
    then do
    saveFile <- tryLoadSaveFile rom'
    return $ rom' { persistent = saveFile }
    else
    return rom'

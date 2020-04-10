module Hardware.Cartridge.Rom
  ( Rom
  , getRomHeader
  , getRom

  , readRom
  )
where

import Control.Monad

import qualified Hardware.Cartridge.Header as Header

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import Utilities.Vector
import Data.Word

import Text.Printf

data Rom = Rom
  { getRomHeader :: Header.Header
  , getRom :: VU.Vector Word8
  }

readRom :: FilePath -> IO (Either String Rom)
readRom fp = do
  bytes <- B.readFile fp
  let vs = byteStringToVector bytes
  return $ do
    when (VU.length vs < 0x8000) $ Left "readRom: file too short"
    when (VU.length vs `mod` 0x4000 /= 0) $ Left $ printf "readRom: file has invalid length 0x%x" (VU.length vs)
    h <- Header.header bytes
    let romBankCount = fromIntegral $ Header.headerRomBanks h
    when (VU.length vs `quot` 0x4000 /= romBankCount) $ Left
      $ printf "readRom: number of banks does not match - should have %i but got %i"
          romBankCount (VU.length vs `quot` 0x4000)
    return (Rom h vs)

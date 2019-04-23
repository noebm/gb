module Cartridge.BootRom
  ( BootRom
  , loadBootRom
  , readBootRom
  )
where

import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString as B

import Data.Word
import Control.Monad

import Utilities.Vector

newtype BootRom = BootRom (VU.Vector Word8)

loadBootRom :: Word16 -> BootRom -> Word8
loadBootRom addr (BootRom xs) = xs VU.! fromIntegral addr

readBootRom :: FilePath -> IO (Either String BootRom)
readBootRom fp = do
  bytes <- B.readFile fp
  let vs = byteStringToVector bytes
  return $ do
    when (VU.length vs /= 0x100) $ Left "readBootRom: invalid length"
    return $ BootRom vs

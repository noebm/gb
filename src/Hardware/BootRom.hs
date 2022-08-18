module Hardware.BootRom
  ( BootRom
  , loadBootRom
  , readBootRom
  ) where

import qualified Data.ByteString               as B
import qualified Data.Vector.Unboxed           as VU

import           Control.Monad
import           Control.Monad.Except
import           Data.Word

import           Utilities.Vector

newtype BootRom = BootRom (VU.Vector Word8)

loadBootRom :: Word16 -> BootRom -> Word8
loadBootRom addr (BootRom xs) = xs VU.! fromIntegral addr

readBootRom :: FilePath -> ExceptT String IO BootRom
readBootRom fp = withExceptT ("readBootRom: " ++) $ do
  bytes <- liftIO $ B.readFile fp
  let vs = byteStringToVector bytes
  when (VU.length vs /= 0x100) $ throwError "invalid length"
  return $ BootRom vs

module BootRom where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed         as VU

import Control.Monad.IO.Class
import Control.Monad.ST

import VectorUtils
import GB

newtype BootRom = BootRom ByteString

readBootRom :: IO BootRom
readBootRom = do
  let bootStrapName = "DMG_ROM.bin"
  fmap BootRom $ B.readFile $ "./" ++ bootStrapName

loadBootRom :: MonadIO m => BootRom -> GB m ()
loadBootRom (BootRom rom) = do
  mem <- unsafeMemory
  liftIO $ stToIO $ VU.copy (VUM.slice 0 0xFF mem) $ byteStringToVector rom
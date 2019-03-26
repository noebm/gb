{-# LANGUAGE FlexibleContexts #-}
module Memory where

-- import Control.Monad.State
-- import Data.Word
import qualified Data.ByteString as B
-- import Control.Lens

type Memory = B.ByteString

memoryBootRom :: IO Memory
memoryBootRom =
  let bootStrapName = "DMG_ROM.bin"
  in B.readFile $ "./" ++ bootStrapName

-- accessMemory :: MonadState Memory m => Word16 -> m Word8
-- accessMemory addr = use (singular (ix (fromIntegral addr)))
-- 
-- writeMemory :: MonadState Memory m => Word16 -> Word8 -> m ()
-- writeMemory addr w = assign (singular (ix (fromIntegral addr))) w

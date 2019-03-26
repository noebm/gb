{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Memory
  ( Memory
  , memoryBootRom
  , accessMemory
  , writeMemory
  )
where

import Control.Monad.State
import Data.Word
import qualified Data.ByteString as B
import Control.Lens

newtype Memory = Memory { _mem :: B.ByteString }

makeLenses ''Memory

memoryBootRom :: IO Memory
memoryBootRom = do
  let bootStrapName = "DMG_ROM.bin"
  b0 <- B.readFile $ "./" ++ bootStrapName
  let b1 = emptyRom 0x00
  return $ Memory $ b0 <> B.drop (B.length b0) b1

accessMemory :: MonadState Memory m => Word16 -> m Word8
accessMemory addr = use (mem . singular (ix (fromIntegral addr)))

writeMemory :: MonadState Memory m => Word16 -> Word8 -> m ()
writeMemory addr = assign (mem . singular (ix (fromIntegral addr)))

emptyRom :: Word8 -> B.ByteString
emptyRom = B.replicate 0x8000

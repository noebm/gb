{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Memory
  -- ( Memory
  -- , mmio
  -- , memoryBootRom
  -- , accessMemory
  -- , writeMemory
  -- )
where

{-
import Control.Monad.State
import Control.Lens

import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)

import MMIO

import Numeric

data Memory = Memory
  { _cartridge   :: B.ByteString
  , _videoRAM    :: Vector Word8 -- B.ByteString
  , _internalRAM :: Vector Word8 -- B.ByteString
  , _zeroRAM     :: Vector Word8 -- includes interrupt register...
  , _oam         :: Vector Word8
  , _mmio :: MMIO
  }

makeLenses ''Memory

memory :: B.ByteString -> Memory
memory cart =
  Memory
  { _cartridge = cart
  , _videoRAM = V.replicate 0x2000 0x00
  , _internalRAM = V.replicate 0x2000 0x00
  , _zeroRAM = V.replicate 0x80 0x00
  , _oam = V.replicate 0xA0 0x00
  , _mmio = defaultMMIO
  }

accessMemory :: MonadState Memory m => Word16 -> m Word8
accessMemory addr
  -- cartridge access
  | addr < 0x8000 = use (cartridge . singular (ix (fromIntegral addr)))
  -- video ram
  | 0x8000 <= addr && addr < 0xA000 = do
    io <- use mmio
    if canAccessVRAM io
      then use (videoRAM    . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
      else return 0xFF
  | 0xA000 <= addr && addr < 0xC000 = error "access to external ram"
  -- internal ram (D000 switchable for cgb)
  | 0xC000 <= addr && addr < 0xE000 = use (internalRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  -- echo ram
  | 0xE000 <= addr && addr < 0xFE00 = use (internalRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  -- OAM
  | 0xFE00 <= addr && addr < 0xFEA0 = do
    io <- use mmio
    if canAccessOAM io
      then use (oam .singular (ix (fromIntegral $ addr .&. 0x9F))) -- error "access to OAM"
      else return 0xFF
  | 0xFF00 <= addr && addr < 0xFF80 = do
      io <- use mmio
      (w , io') <- runStateT (accessMMIO addr) io
      assign mmio io'
      return w
  | 0xFF80 <= addr {- && addr < 0xFFFF -} = use (zeroRAM     . singular (ix (fromIntegral $ addr .&. 0x7F)))
  | otherwise = error $ "access to " ++ showHex addr ""

writeMemory :: MonadState Memory m => Word16 -> Word8 -> m ()
writeMemory addr
  | addr < 0x8000 = assign (cartridge . singular (ix (fromIntegral addr)))
  | 0x8000 <= addr && addr < 0xA000 = \w -> do
    io <- use mmio
    when (canAccessVRAM io) $ assign (videoRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF))) w
  | 0xA000 <= addr && addr < 0xC000 = error "write to external ram"
  | 0xC000 <= addr && addr < 0xE000 = assign (internalRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  | 0xE000 <= addr && addr < 0xFE00 = assign (internalRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  | 0xFE00 <= addr && addr < 0xFEA0 = \w -> do
      io <- use mmio
      when (canAccessOAM io) $ assign (oam .singular (ix (fromIntegral $ addr .&. 0x9F))) w
  | 0xFF00 <= addr && addr < 0xFF80 = \w -> do
      io <- use mmio
      io' <- execStateT (writeMMIO addr w) io
      assign mmio io'
  | 0xFF80 <= addr {- && addr < 0xFFFF -} = assign (zeroRAM     . singular (ix (fromIntegral $ addr .&. 0x7F)))
  | otherwise = const (error $ "write to " ++ showHex addr "")

emptyRom :: Word8 -> B.ByteString
emptyRom = B.replicate 0x8000

-}

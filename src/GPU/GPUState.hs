module GPU.GPUState
  ( module X
  , GPUState (..)
  , GPURequest(..)
  , defaultGPUState
  , updateGPUState
  , dmaTransfer
  , loadGPU
  , storeGPU
  , inGPURange
  , VideoRAM
  , OAM
  )
where

import Control.Lens
import GPU.Memory
import GPU.GPUControl as X

import Control.Monad
import Data.Word
import Data.Bits

-- only used for dmaTransfer
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector)

data GPUState = GPUState
  { gpuVideoRAM        :: !VideoRAM
  , gpuOAM             :: !OAM
  , gpuConfig          :: GPUControl
  }

defaultGPUState :: GPUState
defaultGPUState = GPUState
  { gpuVideoRAM        = defaultVideoRAM
  , gpuOAM             = defaultOAM
  , gpuConfig          = defaultGPUControl
  }

updateGPUState :: Word -> GPUState -> (Bool, Maybe GPURequest, GPUState)
updateGPUState cycles s = do
  if view gpuEnabled (gpuConfig s) then
    let (f, req, c) = updateGPUControl cycles (gpuConfig s)
    in (f , req, s { gpuConfig = c })
    else (False, Nothing, s)

inVideoRAM, inOAM, inGPUMMIO, inGPURange :: (Num a, Ord a) => a -> Bool
inVideoRAM addr = 0x8000 <= addr && addr < 0xA000
inOAM      addr = 0xFE00 <= addr && addr < 0xFF00
inGPUMMIO  addr = 0xFF40 <= addr && addr < 0xFF50
inGPURange addr = inVideoRAM addr || inOAM addr || inGPUMMIO addr
{-# INLINE inVideoRAM #-}
{-# INLINE inOAM #-}
{-# INLINE inGPUMMIO #-}
{-# INLINE inGPURange #-}

loadGPU :: GPUState -> Word16 -> Word8
loadGPU g addr
  | inVideoRAM addr = maybe 0xff id $ do
      guard (_gpuMode (gpuConfig g) /= ModeVRAM)
      return $ loadVideoRAM (gpuVideoRAM g) addr
  | inOAM addr      = maybe 0xff id $ do
      guard (_gpuMode (gpuConfig g) /= ModeVRAM || _gpuMode (gpuConfig g) /= ModeOAM )
      return $ loadOAM (gpuOAM g) addr
  | inGPUMMIO addr  = loadGPUControl conf addr
  | otherwise = error "loadGPU: not in range"
  where conf = gpuConfig g

-- since we dont have access to MonadEmulator yet
-- this seems the best way
dmaTransfer :: Monad m => (Word16 -> m Word8) -> Word16 -> GPUState -> m GPUState
dmaTransfer access baseaddr g = do
  vec <- VU.generateM 0xa0 $ access . fromIntegral . (fromIntegral baseaddr +)
  return $ g { gpuOAM = OAM vec }

storeGPU :: GPUState -> Word16 -> Word8 -> GPUState
storeGPU g@GPUState { gpuConfig = conf } addr b
  | inVideoRAM addr = maybe g (\x -> g { gpuVideoRAM = x }) $ do
      guard (_gpuMode (gpuConfig g) /= ModeVRAM)
      return $ storeVideoRAM (gpuVideoRAM g) addr b
  | inOAM addr && addr /= 0xff46 = maybe g (\oam -> g { gpuOAM = oam }) $ do
      guard (_gpuMode (gpuConfig g) /= ModeVRAM || _gpuMode (gpuConfig g) /= ModeOAM )
      return $ storeOAM (gpuOAM g) addr b
  | inGPUMMIO addr = g { gpuConfig = storeGPUControl conf addr b }
  | otherwise = error "storeGPU: not in range"

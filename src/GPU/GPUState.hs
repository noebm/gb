module GPU.GPUState
  ( module X
  , GPUState (..)
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

updateGPUState :: Word -> GPUState -> (Bool, GPUState)
updateGPUState cycles s = do
  if view gpuEnabled (gpuConfig s) then
    let (f, c) = updateGPUControl cycles (gpuConfig s)
    in (f , s { gpuConfig = c })
    else (False, s)

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
  | inVideoRAM addr = maybe 0xff id $ loadVideoRAM (gpuConfig g) (gpuVideoRAM g) addr
  | inOAM addr      = maybe 0xff id $ loadOAM conf (gpuOAM g) addr
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
  | inVideoRAM addr =
    maybe g (\x -> g { gpuVideoRAM = x }) $ storeVideoRAM conf (gpuVideoRAM g) addr b
  | inOAM addr && addr /= 0xff46 =
    maybe g (\oam -> g { gpuOAM = oam }) $ storeOAM conf (gpuOAM g) addr b
  | inGPUMMIO addr = g { gpuConfig = storeGPUControl conf addr b }
  | otherwise = error "storeGPU: not in range"

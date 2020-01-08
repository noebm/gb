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

import GPU.Memory
import GPU.GPUConfig as X

import Control.Monad
import Data.Word
import Data.Bits

-- only used for dmaTransfer
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector)

-- stores updates as deltas until needed
data GPUState = GPUState
  { gpuVideoRAM        :: !VideoRAM
  , gpuVideoRAMUpdates :: ![ MemoryUpdate ]
  , gpuOAM             :: !OAM
  , gpuOAMUpdates      :: ![ MemoryUpdate ]
  , gpuConfig          :: GPUConfig
  }

defaultGPUState :: GPUState
defaultGPUState = GPUState
  { gpuVideoRAM        = defaultVideoRAM
  , gpuVideoRAMUpdates = []
  , gpuOAM             = defaultOAM
  , gpuOAMUpdates      = []
  , gpuConfig          = defaultGPUConfig
  }

updateVideoRAMState, updateOAMState :: GPUState -> GPUState
updateVideoRAMState g = g
  { gpuVideoRAM = updateVideoRAM (gpuVideoRAMUpdates g) (gpuVideoRAM g)
  , gpuVideoRAMUpdates = []
  }
updateOAMState g = g
  { gpuOAM = updateOAM (gpuOAMUpdates g) (gpuOAM g)
  , gpuOAMUpdates = []
  }

updateGPUState :: Word -> GPUState -> (Bool, GPUState)
updateGPUState cycles s = do
  if _gpuEnabled (gpuConfig s) then
    let (f, c) = updateGPUConfig cycles (gpuConfig s)
    in (f , updateVideoRAMState $ updateOAMState $ s { gpuConfig = c })
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

loadGPU :: GPUState -> Word16 -> (Word8 , Maybe GPUState)
loadGPU g addr
  | inVideoRAM addr =
    let g' = updateVideoRAMState g
        ram' = gpuVideoRAM g'
    in maybe (0xff, Nothing) (\x -> (x , Just g')) $ loadVideoRAM conf ram' addr
  | inOAM addr =
    let g' = updateOAMState g
        oam' = gpuOAM g'
    in maybe (0xff, Nothing) (\x -> (x , Just g')) $ loadOAM conf oam' addr
  | inGPUMMIO addr = (loadGPUConfig conf addr , Nothing)
  | otherwise = error "loadGPU: not in range"
  where conf = gpuConfig g

-- since we dont have access to MonadEmulator yet
-- this seems the best way
dmaTransfer :: Monad m => (Word16 -> m Word8) -> Word16 -> GPUState -> m GPUState
dmaTransfer access baseaddr g = do
  vec <- VU.generateM 0xa0 $ access . fromIntegral . (fromIntegral baseaddr +)
  return $ g { gpuOAMUpdates = [] , gpuOAM = OAM vec }

storeGPU :: GPUState -> Word16 -> Word8 -> GPUState
storeGPU g@GPUState { gpuConfig = conf } addr b
  | inVideoRAM addr =
    maybe g (\x -> g { gpuVideoRAMUpdates = x : gpuVideoRAMUpdates g })
    $ storeVideoRAM conf addr b
  | inOAM addr && addr /= 0xff46 =
    maybe g (\x -> g { gpuOAMUpdates = x : gpuOAMUpdates g })
    $ storeOAM conf addr b
  | inGPUMMIO addr = g { gpuConfig = storeGPUConfig conf addr b }
  | otherwise = error "storeGPU: not in range"

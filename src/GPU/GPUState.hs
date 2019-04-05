module GPU.GPUState
  ( module X
  , GPUState (..)
  , updateGPUState
  , loadGPU
  , storeGPU
  , inGPURange
  , VideoRAM
  , OAM
  )
where

import GPU.Memory
  ( MemoryUpdate
  , VideoRAM
  , loadVideoRAM
  , storeVideoRAM
  , updateVideoRAM

  , OAM
  , loadOAM
  , storeOAM
  , updateOAM
  )
import GPU.GPUConfig as X

import Data.Word

-- stores updates as deltas until needed
data GPUState = GPUState
  { gpuVideoRAM        :: VideoRAM
  , gpuVideoRAMUpdates :: [ MemoryUpdate ]
  , gpuOAM             :: OAM
  , gpuOAMUpdates      :: [ MemoryUpdate ]
  , gpuConfig          :: GPUConfig
  }

updateGPUConfigState :: Word -> GPUState -> Maybe (Word, GPUState)
updateGPUConfigState cycles s
   = (\(cycles' , conf) -> (cycles' , s { gpuConfig = conf }))
  <$> updateGPUConfig cycles (gpuConfig s)

updateVideoRAMState, updateOAMState :: GPUState -> GPUState
updateVideoRAMState g = g
  { gpuVideoRAM = updateVideoRAM (gpuVideoRAMUpdates g) (gpuVideoRAM g)
  , gpuVideoRAMUpdates = []
  }
updateOAMState g = g
  { gpuOAM = updateOAM (gpuOAMUpdates g) (gpuOAM g)
  , gpuOAMUpdates = []
  }

updateGPUState :: Word -> GPUState -> Maybe (Word, GPUState)
updateGPUState cycles s = do
  (cycles' , s') <- updateGPUConfigState cycles s
  return (cycles' , updateVideoRAMState $ updateOAMState s')

inVideoRAM :: Word16 -> Bool
inVideoRAM addr = 0x8000 <= addr && addr < 0xA000

inOAM :: Word16 -> Bool
inOAM addr = 0xFE00 <= addr && addr < 0xFF00

inGPUMMIO :: Word16 -> Bool
inGPUMMIO addr =  0xFF40 <= addr && addr < 0xFF50

inGPURange :: Word16 -> Bool
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

storeGPU :: GPUState -> Word16 -> Word8 -> GPUState
storeGPU g@(GPUState { gpuConfig = conf }) addr b
  | inVideoRAM addr =
    maybe g (\x -> g { gpuVideoRAMUpdates = x : gpuVideoRAMUpdates g })
    $ storeVideoRAM conf addr b
  | inOAM addr =
    maybe g (\x -> g { gpuOAMUpdates = x : gpuOAMUpdates g })
    $ storeOAM conf addr b
  | inGPUMMIO addr = g { gpuConfig = storeGPUConfig conf addr b }
  | otherwise = error "storeGPU: not in range"

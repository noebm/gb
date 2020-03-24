module Hardware.GPU.GPUState
  ( module X
  , GPUState (..)
  , GPURequest(..)
  , defaultGPUState
  , updateGPUState
  , updateGPUState'
  , dmaTransfer
  , VideoRAM
  , OAM
  , Frame

  , loadGPURAM, loadGPUOAM, loadGPURegisters
  , storeGPURAM, storeGPUOAM, storeGPURegisters
  )
where

import Control.Lens
import Hardware.GPU.Memory
import Hardware.GPU.GPUControl as X
import Hardware.GPU.Sprite
import Hardware.GPU.Frame

import Control.Monad
import Data.Word

-- only used for dmaTransfer
import qualified Data.Vector.Unboxed as VU

data GPUState = GPUState
  { gpuVideoRAM        :: !VideoRAM
  , gpuOAM             :: !OAM
  , gpuConfig          :: GPUControl
  , frameBuffer        :: Frame
  }

defaultGPUState :: GPUState
defaultGPUState = GPUState
  { gpuVideoRAM        = defaultVideoRAM
  , gpuOAM             = defaultOAM
  , gpuConfig          = defaultGPUControl
  , frameBuffer        = newFrame
  }

updateGPUState :: Word -> GPUState -> (Bool, Maybe GPURequest, GPUState)
updateGPUState cycles s =
  if view gpuEnabled (gpuConfig s) then
    let (f, req, c) = updateGPUControl cycles (gpuConfig s)
    in (f , req, s { gpuConfig = c })
    else (False, Nothing, s)

updateGPUState' :: Word -> GPUState -> (Bool, Maybe Frame, GPUState)
updateGPUState' cycles s =
  if view gpuEnabled (gpuConfig s) then
    let (f , req, c) = updateGPUControl cycles (gpuConfig s)
        s' = s { gpuConfig = c }
    in case req of
      Just Draw -> (f , guard (frameDone (frameBuffer s)) *> Just (frameBuffer s) , s' { frameBuffer = newFrame })
      Just NewLine ->
        (f , Nothing, s' { frameBuffer = updateFrame c (gpuVideoRAM s) (gpuOAM s) (frameBuffer s) })
      Nothing -> (f , Nothing, s')
    else (False, Nothing, s)

loadGPURAM :: Word16 -> GPUState -> Word8
loadGPURAM addr g = maybe 0xff id $ do
  guard (_gpuMode (gpuConfig g) /= ModeVRAM)
  return $ loadVideoRAM (gpuVideoRAM g) addr

loadGPUOAM :: Word16 -> GPUState -> Word8
loadGPUOAM addr g = maybe 0xff id $ loadOAM (gpuConfig g) (gpuOAM g) addr

loadGPURegisters :: Word16 -> GPUState -> Word8
loadGPURegisters addr g = loadGPUControl (gpuConfig g) addr

-- since we dont have access to MonadEmulator yet
-- this seems the best way
dmaTransfer :: Monad m => (Word16 -> m Word8) -> Word16 -> GPUState -> m GPUState
dmaTransfer access baseaddr g = do
  vec <- VU.generateM 0xa0 $ access . fromIntegral . (fromIntegral baseaddr +)
  return $ g { gpuOAM = directMemoryAccessOAM vec }

storeGPURAM :: Word16 -> Word8 -> GPUState -> GPUState
storeGPURAM addr b g = maybe g (\x -> g { gpuVideoRAM = x }) $ do
  guard (_gpuMode (gpuConfig g) /= ModeVRAM)
  return $ storeVideoRAM (gpuVideoRAM g) addr b

storeGPUOAM :: Word16 -> Word8 -> GPUState -> GPUState
storeGPUOAM addr b g
  = maybe g (\oam -> g { gpuOAM = oam }) $ storeOAM (gpuConfig g) addr b (gpuOAM g)

storeGPURegisters :: Word16 -> Word8 -> GPUState -> GPUState
storeGPURegisters addr b g = g { gpuConfig = storeGPUControl (gpuConfig g) addr b }

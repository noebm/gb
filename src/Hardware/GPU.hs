module Hardware.GPU
  ( module X
  , GPUState (..)
  , GPURequest(..)
  , defaultGPUState
  , updateGPUState

  , VideoRAM
  , OAM
  , Frame

  , loadGPURAM, loadGPUOAM, loadGPURegisters
  , storeGPURAM, storeGPUOAM, storeGPURegisters

  , fillGPUOAMUnsafe
  )
where

import Hardware.GPU.GPUControl as X
import Hardware.GPU.VideoRAM
import Hardware.GPU.OAM
import Hardware.GPU.Frame

import Control.Monad
import Data.Word

import Data.STRef
import Control.Monad.ST

import qualified Data.Vector.Unboxed as VU

data GPUState s = GPUState
  { gpuVideoRAM        :: STRef s VideoRAM
  , gpuOAM             :: STRef s OAM
  , gpuConfig          :: STRef s GPUControl
  , frameBuffer        :: STRef s FrameBuffer
  }

defaultGPUState :: ST s (GPUState s)
defaultGPUState = GPUState
  <$> newSTRef defaultVideoRAM
  <*> newSTRef defaultOAM
  <*> newSTRef defaultGPUControl
  <*> newSTRef newFrameBuffer

updateGPUState :: Word -> GPUState s -> ST s (Bool, Maybe Frame)
updateGPUState cycles s = do
    gctrl <- readSTRef (gpuConfig s)
    let (f , req, c) = updateGPUControl cycles gctrl
    writeSTRef (gpuConfig s) c
    (,) f <$> case req of
      Just Draw -> do
        frame <- getFrame <$> readSTRef (frameBuffer s)
        writeSTRef (frameBuffer s) newFrameBuffer
        return $ Just frame
      Just NewLine -> do
        vram <- readSTRef (gpuVideoRAM s)
        oam <- readSTRef (gpuOAM s)
        modifySTRef (frameBuffer s) $ updateFrameBuffer c vram oam
        return Nothing
      Nothing -> return Nothing

loadGPURAM :: Word16 -> GPUState s -> ST s Word8
loadGPURAM addr g = do
  gctrl <- readSTRef (gpuConfig g)
  maybe (return 0xff) id $ do
    guard (canAccessGPURAM gctrl)
    return $ loadVideoRAM <$> readSTRef (gpuVideoRAM g) <*> pure addr

loadGPUOAM :: Word16 -> GPUState s -> ST s Word8
loadGPUOAM addr s = do
  gctrl <- readSTRef (gpuConfig s)
  maybe (return 0xff) id $ do
    guard (canAccessOAM gctrl)
    return $ loadOAM addr <$> readSTRef (gpuOAM s)

loadGPURegisters :: Word16 -> GPUState s -> ST s Word8
loadGPURegisters addr s = loadGPUControl addr <$> readSTRef (gpuConfig s)

storeGPURAM :: Word16 -> Word8 -> GPUState s -> ST s ()
storeGPURAM addr b g = do
  gctrl <- readSTRef (gpuConfig g)
  when (canAccessGPURAM gctrl) $
    modifySTRef (gpuVideoRAM g) $ storeVideoRAM addr b

storeGPUOAM :: Word16 -> Word8 -> GPUState s -> ST s ()
storeGPUOAM addr b g = do
  gctrl <- readSTRef (gpuConfig g)
  when (canAccessOAM gctrl) $
    modifySTRef (gpuOAM g) $ storeOAM addr b

fillGPUOAMUnsafe :: OAM -> GPUState s -> ST s ()
fillGPUOAMUnsafe oam g = writeSTRef (gpuOAM g) oam

storeGPURegisters :: Word16 -> Word8 -> GPUState s -> ST s ()
storeGPURegisters addr b g = modifySTRef (gpuConfig g) $ storeGPUControl addr b

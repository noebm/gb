{-# LANGUAGE RankNTypes #-}
module Hardware.GPU.Frame where

import Control.Monad.ST
import Control.Lens

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Word

import Hardware.GPU.GPUControl
import Hardware.GPU.Drawing

newtype Frame = Frame (V.Vector Word8)

getFrame :: FrameBuffer -> Frame
getFrame (FrameBuffer v) = Frame $ V.create v

newtype FrameBuffer = FrameBuffer (forall s . ST s (VM.MVector s Word8))

newFrameBuffer :: FrameBuffer
newFrameBuffer = FrameBuffer $ VM.new (144 * 160)

updateFrameBuffer :: GPUControl -> VideoRAM -> OAM -> FrameBuffer -> FrameBuffer
updateFrameBuffer gctrl vram oam (FrameBuffer buffer) = FrameBuffer $ do
  let start = 160 * fromIntegral (gctrl ^. gpuLine)
  v <- buffer
  V.copy (VM.slice start 160 v) (generateLine gctrl vram oam)
  return v

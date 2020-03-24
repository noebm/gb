module Hardware.GPU.Frame where

import Hardware.GPU.Drawing
import Hardware.GPU.Sprite
import Hardware.GPU.Memory
import Hardware.GPU.GPUControl

import qualified Data.Vector.Unboxed as V
import Data.Word

newtype Frame = Frame (V.Vector Word8)

newFrame :: Frame
newFrame = Frame V.empty

frameDone :: Frame -> Bool
frameDone (Frame v) = V.length v == 144 * 160

updateFrame :: GPUControl -> VideoRAM -> OAM -> Frame -> Frame
updateFrame gctrl vram oam (Frame v) = Frame $ v V.++ generateLine gctrl vram oam

module GPU.Drawing where

import Data.Word
import Data.Bits

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector)

import GPU.Memory
import GPU.GPUConfig
import GPU.Palette

-- -- just a wrapper to make sure value is in desired range
-- newtype VideoAddr = VideoAddr Int

backgroundLine :: GPUConfig -> VideoRAM -> Word8 -> Vector ColorCode
backgroundLine g vram y = VU.generate 160 $ \x ->
  let x' = fromIntegral x + gpuScrollX g
      y' =              y + gpuScrollY g
      idx = loadVideoRAM' vram (backgroundTableIndex g x' y')
  in loadTile (tile vram (tileAddress g idx)) x' y'

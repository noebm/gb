module GPU.VideoAddr where

import GPU.GPUConfig
import Data.Word
import Data.Bits

-- just a wrapper to make sure value is in desired range
newtype VideoAddr = VideoAddr Int
  deriving Show

{-# INLINE backgroundTableIndex #-}
backgroundTableIndex :: GPUConfig -> Word8 -> Word8 -> VideoAddr
backgroundTableIndex GPUConfig { gpuBGTileMapSelect = select } col row =
  let bgrdTableIndex = fromIntegral col + 32 * fromIntegral row
      bgrdTableBase = if select then 0x9C00 else 0x9800
  in VideoAddr $ (bgrdTableBase + bgrdTableIndex) .&. 0x1fff

{-# INLINE tileAddress #-}
tileAddress :: GPUConfig -> Word8 -> VideoAddr
tileAddress g idx = VideoAddr $ 0x1fff .&. if gpuTileDataSelect g
  then 0x8000 + fromIntegral idx `shiftL` 4
  else 0x8800 + fromIntegral (idx + 128) `shiftL` 4

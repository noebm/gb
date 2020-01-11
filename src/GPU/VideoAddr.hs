module GPU.VideoAddr where

import Data.Word
import Data.Bits
import Control.Lens

newtype TileMapAddr = TileMapAddr Word16

{-# INLINE tileTableIndex #-}
tileTableIndex :: Bool -> Word8 -> Word8 -> TileMapAddr
tileTableIndex tileMapSelect col row =
  let bgrdTableIndex = fromIntegral col + 32 * fromIntegral row
      bgrdTableBase = if tileMapSelect then 0x9C00 else 0x9800
  in TileMapAddr $ (bgrdTableBase + bgrdTableIndex) .&. 0x1fff

newtype TileAddr = TileAddr Word16

{-# INLINE tileAddr #-}
tileAddr :: Bool -> Word8 -> TileAddr
tileAddr tileDataSelect idx = TileAddr $ 0x1fff .&. if tileDataSelect
  then 0x8000 + fromIntegral idx `shiftL` 4
  else 0x8800 + fromIntegral (idx + 128) `shiftL` 4

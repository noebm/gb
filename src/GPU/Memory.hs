module GPU.Memory where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector)

import Data.Word
import Data.Bits

import GPU.GPUConfig
import GPU.Palette

import Data.Monoid

newtype OAM = OAM { getOAM :: Vector Word8 }

newtype VideoRAM = VideoRAM { getVideoRAM :: Vector Word8 }

-- just a wrapper to make sure value is in desired range
newtype VideoAddr = VideoAddr Int

backgroundTableIndex :: GPUConfig -> Word8 -> Word8 -> VideoAddr
backgroundTableIndex GPUConfig { gpuBGTileMapSelect = select } x y =
  let bgrdTableIndex = fromIntegral (x `div` 8) + 32 * fromIntegral (y `div` 8)
      bgrdTableBase = if select then 0x9C00 else 0x9800
  in VideoAddr (bgrdTableBase + bgrdTableIndex .&. 0x1fff)

tileAddress :: GPUConfig -> Word8 -> VideoAddr
tileAddress g idx = VideoAddr $ 0x1ffff .&. if gpuTileDataSelect g
  then 0x8000 + fromIntegral idx `shiftL` 4
  else 0x8800 + fromIntegral (idx + 128) `shiftL` 4

backgroundLine :: GPUConfig -> VideoRAM -> Word8 -> Vector Word8
backgroundLine g vram y = VU.generate 160 $ \x ->
  let y' = y + gpuScrollY g
      x' = fromIntegral x + gpuScrollX g
      idx = loadVideoRAM vram (backgroundTableIndex g x' y')
  in undefined

newtype Tile = Tile (Vector Word8)

{-# INLINE tile #-}
tile :: VideoRAM -> VideoAddr -> Tile
tile (VideoRAM vram) (VideoAddr addr) = Tile $ VU.slice addr tilesize vram
  where tilesize = 2 * 8 -- byte

loadTile :: Tile -> Word8 -> Word8 -> ColorCode
loadTile (Tile t) x y = fromIntegral $ foldr (\v acc -> (acc `shiftL` 1) .|. v) 0x00
  [ fromEnum (b `testBit` bitOffset) | b <- (t VU.!) <$> [byteOffset .. byteOffset + 1] ]
  where byteOffset    = fromIntegral $ (y .&. 7) `shiftL` 1
        bitOffset = fromIntegral $ complement x .&. 7

{-# INLINE loadVideoRAM #-}
loadVideoRAM :: VideoRAM -> VideoAddr -> Word8
loadVideoRAM (VideoRAM m) (VideoAddr addr) = m VU.! addr

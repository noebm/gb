module Hardware.GPU.Memory
  ( tileTableIndex

  , Tile
  , getTile
  , getTileColor

  , getTileAddr

  , VideoRAM
  , defaultVideoRAM
  , loadVideoRAM
  , storeVideoRAM
  , dumpVideoRAM

  )
where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Vector.Unboxed (Vector)
import Control.Lens

import Data.Word
import Data.Bits
import Data.Bits.Lens

import Hardware.GPU.Palette (Color(..))
import Hardware.GPU.VideoAddr

newtype VideoRAM = VideoRAM (Vector Word8)

defaultVideoRAM :: VideoRAM
defaultVideoRAM = VideoRAM $ VU.replicate 0x2000 0x00

dumpVideoRAM :: VideoRAM -> [ Word8 ]
dumpVideoRAM (VideoRAM m) = VU.toList m

newtype Tile = Tile (Vector Word8)

{-# INLINE getTile #-}
getTile :: VideoRAM -> TileAddr -> Tile
getTile (VideoRAM vram) (TileAddr addr) = Tile $ VU.slice (fromIntegral addr) tilesize vram
  where tilesize = 2 * 8 -- byte

{-# INLINE getTileColor #-}
getTileColor :: Tile -> Word8 -> Word8 -> Color
getTileColor (Tile t) x y = Color
  $ 0x00 & bitAt 0 .~ (byte1 ^. bitAt bitOffset) & bitAt 1 .~ (byte2 ^. bitAt bitOffset)
  where byteOffset = fromIntegral $ (y .&. 7) `shiftL` 1
        bitOffset  = fromIntegral $ complement x .&. 7
        byte1 = t VU.! byteOffset
        byte2 = t VU.! (byteOffset + 1)

{-# INLINE getTileAddr #-}
getTileAddr :: Bool -> VideoRAM -> TileMapAddr -> TileAddr
getTileAddr tileDataSelect (VideoRAM m) (TileMapAddr addr) = tileAddr tileDataSelect $ m VU.! fromIntegral addr

{-
external interface
-}

{-# INLINE loadVideoRAM #-}
loadVideoRAM :: VideoRAM -> Word16 -> Word8
loadVideoRAM (VideoRAM m) addr = m VU.! fromIntegral (addr .&. 0x1fff)

{-# INLINE storeVideoRAM #-}
storeVideoRAM :: VideoRAM -> Word16 -> Word8 -> VideoRAM
storeVideoRAM (VideoRAM m) addr b = VideoRAM $ VU.modify (\v -> VUM.write v (fromIntegral (addr .&. 0x1fff)) b) m

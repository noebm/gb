module GPU.Memory
  ( tileTableIndex
  , tileAddress

  , Tile
  , tile
  , loadTile

  , loadVideoRAM'

  , MemoryUpdate
  , updateVideoRAM
  , updateOAM

  , VideoRAM
  , defaultVideoRAM
  , loadVideoRAM
  , storeVideoRAM
  , dumpVideoRAM

  , OAM (..)
  , defaultOAM
  , loadOAM
  , storeOAM
  , dumpOAM
  )
where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector)
import Control.Monad
import Control.Lens

import Data.Word
import Data.Bits
import Data.Bits.Lens

import GPU.GPUControl
import GPU.Palette
import GPU.VideoAddr

newtype OAM = OAM (Vector Word8)

newtype VideoRAM = VideoRAM (Vector Word8)

defaultVideoRAM :: VideoRAM
defaultVideoRAM = VideoRAM $ VU.replicate 0x2000 0x00

defaultOAM :: OAM
defaultOAM = OAM $ VU.replicate 0xa0 0x00

updateVideoRAM :: [ MemoryUpdate ] -> VideoRAM -> VideoRAM
updateVideoRAM xs (VideoRAM m) = VideoRAM $! m VU.// reverse xs

updateOAM :: [ MemoryUpdate ] -> OAM -> OAM
updateOAM xs (OAM m) = OAM $! m VU.// reverse xs

dumpOAM :: OAM -> [ Word8 ]
dumpOAM (OAM m) = VU.toList m

dumpVideoRAM :: VideoRAM -> [ Word8 ]
dumpVideoRAM (VideoRAM m) = VU.toList m

type MemoryUpdate = (Int, Word8)

newtype Tile = Tile (Vector Word8)

{-# INLINE tile #-}
tile :: VideoRAM -> VideoAddr -> Tile
tile (VideoRAM vram) (VideoAddr addr) = Tile $ VU.slice addr tilesize vram
  where tilesize = 2 * 8 -- byte

{-# INLINE loadTile #-}
loadTile :: Tile -> Word8 -> Word8 -> Color
loadTile (Tile t) x y = Color
  $ 0x00 & bitAt 0 .~ (byte1 ^. bitAt bitOffset) & bitAt 1 .~ (byte2 ^. bitAt bitOffset)
  where byteOffset = fromIntegral $ (y .&. 7) `shiftL` 1
        bitOffset  = fromIntegral $ complement x .&. 7
        byte1 = t VU.! byteOffset
        byte2 = t VU.! (byteOffset + 1)

{-# INLINE loadVideoRAM' #-}
loadVideoRAM' :: VideoRAM -> VideoAddr -> Word8
loadVideoRAM' (VideoRAM m) (VideoAddr addr) = m VU.! addr

{-
external interface
-}

{-# INLINE loadVideoRAM #-}
loadVideoRAM :: GPUControl -> VideoRAM -> Word16 -> Maybe Word8
loadVideoRAM GPUControl { _gpuMode = mode } (VideoRAM m) addr = do
  guard (mode /= ModeVRAM)
  VU.indexM m $ fromIntegral (addr .&. 0x1fff)

{-# INLINE storeVideoRAM #-}
storeVideoRAM :: GPUControl -> Word16 -> Word8 -> Maybe MemoryUpdate
storeVideoRAM GPUControl { _gpuMode = mode } addr b = do
  guard (mode /= ModeVRAM)
  return ( fromIntegral addr .&. 0x1fff , b )

{-# INLINE loadOAM #-}
loadOAM :: GPUControl -> OAM -> Word16 -> Maybe Word8
loadOAM GPUControl { _gpuMode = mode } (OAM m) addr = do
  guard (mode /= ModeVRAM || mode /= ModeOAM)
  VU.indexM m $ fromIntegral (addr .&. 0x9f)

{-# INLINE storeOAM #-}
storeOAM :: GPUControl -> Word16 -> Word8 -> Maybe MemoryUpdate
storeOAM GPUControl { _gpuMode = mode } addr b = do
  guard (mode /= ModeVRAM || mode /= ModeOAM)
  return ( fromIntegral addr .&. 0x9f , b )

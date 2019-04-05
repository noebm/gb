module GPU.Memory
  ( backgroundTableIndex
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

  , OAM
  , defaultOAM
  , loadOAM
  , storeOAM

  , dmaTransfer
  )
where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector)
import Control.Monad

import Data.Word
import Data.Bits

import GPU.GPUConfig
import GPU.Palette
import GPU.VideoAddr

newtype OAM = OAM { getOAM :: Vector Word8 }

newtype VideoRAM = VideoRAM { getVideoRAM :: Vector Word8 }

defaultVideoRAM :: VideoRAM
defaultVideoRAM = VideoRAM $ VU.replicate 0x2000 0x00

defaultOAM :: OAM
defaultOAM = OAM $ VU.replicate 0xa0 0x00

updateVideoRAM :: [ MemoryUpdate ] -> VideoRAM -> VideoRAM
updateVideoRAM xs (VideoRAM m) = VideoRAM $! m VU.// xs

updateOAM :: [ MemoryUpdate ] -> OAM -> OAM
updateOAM xs (OAM m) = OAM $! m VU.// xs

type MemoryUpdate = (Int, Word8)


newtype Tile = Tile (Vector Word8)

{-# INLINE tile #-}
tile :: VideoRAM -> VideoAddr -> Tile
tile (VideoRAM vram) (VideoAddr addr) = Tile $ VU.slice addr tilesize vram
  where tilesize = 2 * 8 -- byte

loadTile :: Tile -> Word8 -> Word8 -> ColorCode
loadTile (Tile t) x y = fromIntegral $ foldr (\v acc -> (acc `shiftL` 1) .|. v) 0x00
  [ fromEnum (b `testBit` bitOffset) | b <- (t VU.!) <$> [byteOffset .. byteOffset + 1] ]
  where byteOffset = fromIntegral $ (y .&. 7) `shiftL` 1
        bitOffset  = fromIntegral $ complement x .&. 7

{-# INLINE loadVideoRAM' #-}
loadVideoRAM' :: VideoRAM -> VideoAddr -> Word8
loadVideoRAM' (VideoRAM m) (VideoAddr addr) = m VU.! addr

{-
external interface
-}

{-# INLINE loadVideoRAM #-}
loadVideoRAM :: GPUConfig -> VideoRAM -> Word16 -> Maybe Word8
loadVideoRAM GPUConfig { gpuMode = mode } (VideoRAM m) addr = do
  guard (mode /= ModeVRAM)
  VU.indexM m $ fromIntegral (addr .&. 0x1fff)

{-# INLINE storeVideoRAM #-}
storeVideoRAM :: GPUConfig -> Word16 -> Word8 -> Maybe MemoryUpdate
storeVideoRAM GPUConfig { gpuMode = mode } addr b = do
  guard (mode /= ModeVRAM)
  return ( fromIntegral addr .&. 0x1fff , b )

{-# INLINE dmaTransfer #-}
dmaTransfer :: Vector Word8 -> OAM
dmaTransfer xs
  | VU.length xs == 0x1F = OAM xs
  | otherwise            = error "dmaTransfer: invalid length"

{-# INLINE loadOAM #-}
loadOAM :: GPUConfig -> OAM -> Word16 -> Maybe Word8
loadOAM GPUConfig { gpuMode = mode } (OAM m) addr = do
  guard (mode /= ModeVRAM || mode /= ModeOAM)
  VU.indexM m $ fromIntegral (addr .&. 0x9f)

{-# INLINE storeOAM #-}
storeOAM :: GPUConfig -> Word16 -> Word8 -> Maybe MemoryUpdate
storeOAM GPUConfig { gpuMode = mode } addr b = do
  guard (mode /= ModeVRAM || mode /= ModeOAM)
  return ( fromIntegral addr .&. 0x9f , b )

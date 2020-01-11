module GPU.Memory
  ( tileTableIndex
  , tileAddress

  , Tile
  , tile
  , getTileColor

  , loadVideoRAM'

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
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Vector.Unboxed (Vector)
import Control.Lens

import Data.Word
import Data.Bits
import Data.Bits.Lens

import GPU.Palette (Color(..))
import GPU.VideoAddr

newtype OAM = OAM (Vector Word8)

newtype VideoRAM = VideoRAM (Vector Word8)

defaultVideoRAM :: VideoRAM
defaultVideoRAM = VideoRAM $ VU.replicate 0x2000 0x00

defaultOAM :: OAM
defaultOAM = OAM $ VU.replicate 0xa0 0x00

dumpOAM :: OAM -> [ Word8 ]
dumpOAM (OAM m) = VU.toList m

dumpVideoRAM :: VideoRAM -> [ Word8 ]
dumpVideoRAM (VideoRAM m) = VU.toList m

newtype Tile = Tile (Vector Word8)

{-# INLINE tile #-}
tile :: VideoRAM -> VideoAddr -> Tile
tile (VideoRAM vram) (VideoAddr addr) = Tile $ VU.slice addr tilesize vram
  where tilesize = 2 * 8 -- byte

{-# INLINE getTileColor #-}
getTileColor :: Tile -> Word8 -> Word8 -> Color
getTileColor (Tile t) x y = Color
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
loadVideoRAM :: VideoRAM -> Word16 -> Word8
loadVideoRAM (VideoRAM m) addr = m VU.! fromIntegral (addr .&. 0x1fff)

{-# INLINE storeVideoRAM #-}
storeVideoRAM :: VideoRAM -> Word16 -> Word8 -> VideoRAM
storeVideoRAM (VideoRAM m) addr b = VideoRAM $ VU.modify (\v -> VUM.write v (fromIntegral (addr .&. 0x1fff)) b) m

{-# INLINE loadOAM #-}
loadOAM :: OAM -> Word16 -> Word8
loadOAM (OAM m) addr = m VU.! fromIntegral (addr .&. 0x9f)

{-# INLINE storeOAM #-}
storeOAM :: OAM -> Word16 -> Word8 -> OAM
storeOAM (OAM m) addr b = OAM $ VU.modify (\v -> VUM.write v (fromIntegral (addr .&. 0x9f)) b) m

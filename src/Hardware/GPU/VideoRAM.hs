{-# LANGUAGE RankNTypes #-}
module Hardware.GPU.VideoRAM
  ( Tile
  , getTile
  , getSpriteTile
  , getTileColor
  , VideoRAM(..)
  , defaultVideoRAM
  , loadVideoRAM
  , storeVideoRAM
  ) where

import           Control.Lens
import qualified Data.Vector.Unboxed           as VU
import           Data.Vector.Unboxed            ( Vector )

import           Data.Bits
import           Data.Bits.Lens
import           Data.Word

import           Hardware.GPU.Palette           ( Color(..) )

newtype TileMap = TileMap (Vector Word8)
newtype TileData = TileData (Vector Word8)

data VideoRAM = VideoRAM
  { tiles :: TileData
  , map1  :: TileMap
  , map2  :: TileMap
  }

defaultTileData :: TileData
defaultTileData = TileData $ VU.replicate 0x1800 0x00

defaultTileMap :: TileMap
defaultTileMap = TileMap $ VU.replicate 0x400 0x00

defaultVideoRAM :: VideoRAM
defaultVideoRAM = VideoRAM defaultTileData defaultTileMap defaultTileMap

newtype Tile = Tile (Vector Word8)

{-# INLINE getTileColor #-}
getTileColor :: Tile -> Word8 -> Word8 -> Color
getTileColor (Tile t) x y =
  Color
    $  0x00
    &  bitAt 0
    .~ (byte1 ^. bitAt bitOffset)
    &  bitAt 1
    .~ (byte2 ^. bitAt bitOffset)
 where
  byteOffset = fromIntegral $ (y .&. 7) `shiftL` 1
  bitOffset  = fromIntegral $ complement x .&. 7
  byte1      = t VU.! byteOffset
  byte2      = t VU.! (byteOffset + 1)

type TileMapSelect = Bool
type TileDataAccessMode = Bool

getTileData :: TileDataAccessMode -> Word8 -> TileData -> Tile
getTileData accessMode tileOffset (TileData mem) =
  let byteOffset = if accessMode
        then fromIntegral tileOffset `shiftL` 4
        else (fromIntegral (tileOffset + 128) `shiftL` 4) + 0x800
  in  Tile $ VU.slice byteOffset 16 mem

getTileIndex :: TileMapSelect -> Word8 -> Word8 -> VideoRAM -> Word8
getTileIndex mapSelect col row vram =
  let mapIndex  = fromIntegral col + 32 * fromIntegral row
      TileMap m = if mapSelect then map2 vram else map1 vram
  in  m VU.! mapIndex

getTile
  :: TileMapSelect -> TileDataAccessMode -> Word8 -> Word8 -> VideoRAM -> Tile
getTile mapSelect dataAccess col row vram =
  let tileIndex = getTileIndex mapSelect col row vram
  in  getTileData dataAccess tileIndex (tiles vram)

getSpriteTile :: Word8 -> TileData -> Tile
getSpriteTile = getTileData True

{-
external interface
-}

tileData :: Word16 -> Lens' TileData Word8
tileData addr = lens getter setter
 where
    -- more correct would be:
    -- if 0x1000 .&. addr => addr .&. 0x17ff
    -- otherwise => addr .&. 0xfff
  address = fromIntegral (addr .&. 0x1fff)
  getter (TileData d) = d ^?! ix address
  setter (TileData d) byte = TileData $ d & ix address .~ byte

tileMap :: Word16 -> Lens' TileMap Word8
tileMap addr = lens getter setter
 where
  address = fromIntegral (addr .&. 0x3ff)
  getter (TileMap d) = d ^?! ix address
  setter (TileMap d) byte = TileMap $ d & ix address .~ byte

{-# INLINE loadVideoRAM #-}
loadVideoRAM :: VideoRAM -> Word16 -> Word8
loadVideoRAM (VideoRAM d m1 m2) addr | addr < 0x8000 = error msg
                                     | addr < 0x9800 = d ^. tileData addr
                                     | addr < 0x9c00 = m1 ^. tileMap addr
                                     | addr < 0xA000 = m2 ^. tileMap addr
                                     | otherwise     = error msg
  where msg = "loadVideoRAM: not in range: " ++ show addr

{-# INLINE storeVideoRAM #-}
storeVideoRAM :: Word16 -> Word8 -> VideoRAM -> VideoRAM
storeVideoRAM addr byte vram
  | addr < 0x8000 = error msg
  | addr < 0x9800 = vram { tiles = tiles vram & tileData addr .~ byte }
  | addr < 0x9c00 = vram { map1 = map1 vram & tileMap addr .~ byte }
  | addr < 0xA000 = vram { map2 = map2 vram & tileMap addr .~ byte }
  | otherwise     = error msg
  where msg = "storeVideoRAM: not in range: " ++ show addr

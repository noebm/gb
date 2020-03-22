module Hardware.GPU.Debug where

import Hardware.GPU.Memory
import Hardware.GPU.Palette
import Hardware.GPU.VideoAddr

import Data.Word

-- complete background indices for the background
completeTileTable :: VideoRAM -> Bool -> [[ TileMapAddr ]]
completeTileTable mem tileMapSelect =
  [ [ tileTableIndex tileMapSelect ix iy | ix <- [0..31]] | iy <- [0..31] ]

completeBackground :: VideoRAM -> Bool -> Bool -> [[ Tile ]]
completeBackground mem tileDataSelect tileMapSelect =
  fmap (getTile mem . getTileAddr tileDataSelect mem)
  <$> completeTileTable mem tileMapSelect

tilesWithIndex :: [[ a ]] -> [[ ((Word8, Word8), a) ]]
tilesWithIndex tss = do
  ts <- tss
  y <- [0..7]
  return $ do
    t <- ts
    x <- [0..7]
    return $ (,) (x , y) t

backgroundPixels :: VideoRAM -> Bool -> Bool -> [[ Color ]]
backgroundPixels mem tileDataSelect tileMapSelect
  = fmap (fmap (\((x,y),t) -> getTileColor t x y))
  $ tilesWithIndex
  $ completeBackground mem tileDataSelect tileMapSelect

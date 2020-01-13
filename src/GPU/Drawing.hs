module GPU.Drawing where

import Data.Word

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import GPU.Memory
import GPU.Palette
import GPU.GPUControl

import SDL.Vect

import Control.Lens
import Control.Monad.Primitive
import Control.Monad

getTile' :: VideoRAM -> Bool -> Bool -> Word8 -> Word8 -> Tile
getTile' mem tileDataSelect tileMapSelect x y = getTile mem
  $ getTileAddr tileDataSelect mem
  $ tileTableIndex tileMapSelect x y

pixel :: VideoRAM -> Bool -> Bool -> V2 Word8 -> Color
pixel mem tileSelect mapSelect (V2 x y) = getTileColor t x y
  where t = getTile' mem tileSelect mapSelect (x `div` 8) (y `div` 8)

backgroundLine :: GPUControl -> VideoRAM -> V.Vector Word8
backgroundLine g vram = V.generate 160 $ \i ->
  paletteValue (_gpuBGPalette g)
  $ pixel vram (g ^. gpuTileDataSelect) (g ^. gpuBGTileMapSelect)
  $ V2 (fromIntegral i) (g ^. gpuLine) + g ^. gpuScroll

windowLine :: GPUControl -> VideoRAM -> (Word8, V.Vector Word8)
windowLine g vram =
  let y' = g ^. gpuLine + g ^. gpuWindow._y
      windowX = view (gpuWindow._x) g - 7
  in (,) windowX $ V.generate (160 - fromIntegral windowX) $ \i ->
    let x' = fromIntegral i + (g ^. gpuScroll._x) + windowX
        x = if x' >= windowX then fromIntegral i else x'
    in paletteValue (_gpuBGPalette g) $
       pixel vram (g ^. gpuTileDataSelect) (g ^. gpuWindowTileMapSelect) (V2 x y')

generateLine :: PrimMonad m => GPUControl -> VideoRAM -> m (V.Vector Word8)
generateLine gctrl mem = do
  pixels <- VM.new 160
  when (gctrl ^. displayBG) $ do
    let bgrd = backgroundLine gctrl mem
    V.copy pixels bgrd
  when (gctrl ^. displayWindow) $ when (gctrl ^. gpuWindow._y <= gctrl ^. gpuLine) $ do
    let (offset, disp) = windowLine gctrl mem
    V.copy (VM.drop (fromIntegral offset) pixels) disp
  V.freeze pixels

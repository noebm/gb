module GPU.Drawing where

import Data.Word

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import GPU.Memory
import GPU.Palette
import GPU.GPUState

import SDL.Video
import SDL.Vect
import Control.Monad.IO.Class

import Utilities.Vector

import Control.Lens
import Control.Monad.Primitive
import Control.Monad

getTile' :: VideoRAM -> Bool -> Bool -> Word8 -> Word8 -> Tile
getTile' mem tileDataSelect tileMapSelect x y = getTile mem
  $ getTileAddr tileDataSelect mem
  $ tileTableIndex tileMapSelect x y

backgroundLine :: GPUControl -> VideoRAM -> V.Vector Word8
backgroundLine g vram =
  let y = g ^. gpuLine + g ^. gpuScroll._y
  in V.generate 160 $ \i ->
    let x = fromIntegral i + (g ^. gpuScroll._x)
        t = getTile' vram (g ^. gpuTileDataSelect) (g ^. gpuBGTileMapSelect) (x `div` 8) (y `div` 8)
    in paletteValue (_gpuBGPalette g) $ getTileColor t x y

windowLine :: GPUControl -> VideoRAM -> (Word8, V.Vector Word8)
windowLine g vram =
  let y' = g ^. gpuLine + g ^. gpuWindow._y
      windowX = view (gpuWindow._x) g - 7
  in (,) windowX $ V.generate (160 - fromIntegral windowX) $ \i ->
    let x' = fromIntegral i + (g ^. gpuScroll._x) + windowX
        x = if x' >= windowX then fromIntegral i else x'
        t = getTile' vram (g ^. gpuTileDataSelect) (g ^. gpuWindowTileMapSelect) (x `div` 8) (y' `div` 8)
    in paletteValue (_gpuBGPalette g) $ getTileColor t x y'

generateLine :: PrimMonad m => GPUControl -> VideoRAM -> m (VM.MVector (PrimState m) Word8)
generateLine gctrl mem = do
  pixels <- VM.new 160
  when (gctrl ^. displayBG) $ do
    let bgrd = backgroundLine gctrl mem
    V.copy pixels bgrd
  when (gctrl ^. displayWindow) $ when (gctrl ^. gpuWindow._y <= gctrl ^. gpuLine) $ do
    let (offset, disp) = windowLine gctrl mem
    V.copy (VM.drop (fromIntegral offset) pixels) disp
  return pixels

updateTextureLine :: MonadIO m => Texture -> Int -> VS.Vector (V4 Word8) -> m ()
updateTextureLine tex line vs = do
  let vs' = VS.unsafeCast vs
  _ <- updateTexture tex
    (Just $ fromIntegral <$> Rectangle (P $ V2 0 line) (V2 160 1))
    (vectorToByteString vs')
    (fromIntegral $ VS.length vs')
  return ()

paletteColorToGrayscale :: Word8 -> V4 Word8
paletteColorToGrayscale w = V4 c c c 0xff
  where
    c = case w of
      0x00 -> 255
      0x01 -> 192
      0x02 -> 96
      _    -> 0

genPixelRow :: (MonadIO m) => Texture -> GPUState -> m ()
genPixelRow im g = do
  let y = _gpuLine $ gpuConfig g
  v <- liftIO $ V.freeze =<< generateLine (gpuConfig g) (gpuVideoRAM g)
  updateTextureLine im (fromIntegral y)
    $ VS.map paletteColorToGrayscale
    $ VS.convert
    $ v

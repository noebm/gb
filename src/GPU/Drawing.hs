module GPU.Drawing where

import Data.Word

import qualified Data.Vector.Storable as VS

import GPU.Memory
import GPU.Palette
import GPU.GPUState

import SDL.Video
import SDL.Vect
import Control.Monad.IO.Class

import Utilities.Vector

import Control.Lens

backgroundLine :: GPUControl -> VideoRAM -> VS.Vector Word8
backgroundLine g vram =
  let y' = (+) <$> view gpuYCoordinate <*> view (gpuScroll._y) $ g
      (sd, sr) = (g ^. gpuScroll._x) `divMod` 8
      tiles
        = fmap (tile vram)
        $ fmap (tileAddress g)
        $ fmap (loadVideoRAM' vram)
        $ fmap (flip (backgroundTableIndex g) (y' `div` 8))
        $ fmap (+ sd)
        $ [0..20]
  in VS.generate 160 $ \x ->
    let x' = fromIntegral x + (g ^. gpuScroll._x)
        t = tiles !! fromIntegral ((x' `div` 8) - sr)
    in paletteValue (_gpuBGPalette g) $ loadTile t x' y'

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
  let y = _gpuYCoordinate $ gpuConfig g
  updateTextureLine im (fromIntegral y)
    $ VS.map paletteColorToGrayscale $ backgroundLine (gpuConfig g) (gpuVideoRAM g)

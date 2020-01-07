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

backgroundLine :: GPUConfig -> VideoRAM -> VS.Vector (V4 Word8)
backgroundLine g vram =
  let y' = gpuYCoordinate g + gpuScrollY g
      (sd, sr) = gpuScrollX g `divMod` 8
      addr
        = fmap (tileAddress g)
        $ fmap (loadVideoRAM' vram)
        $ fmap (flip (backgroundTableIndex g) y')
        $ fmap (8 *)
        $ fmap (+ sd)
        $ [0..20]
  in VS.generate 160 $ \x ->
    let x' = fromIntegral x + gpuScrollX g
        (xd, xr) = fromIntegral x `divMod` 8
        offset = (xr + sr) `div` 8
        t = tile vram (addr !! fromIntegral (xd + offset))
    in (\c -> V4 c c c 0xff) . paletteGrayscale (gpuBGPalette g)
       $ loadTile t x' y'

updateTextureLine :: MonadIO m => Texture -> Int -> VS.Vector (V4 Word8) -> m ()
updateTextureLine tex line vs = do
  let vs' = VS.unsafeCast vs
  _ <- updateTexture tex
    (Just $ fromIntegral <$> Rectangle (P $ V2 0 line) (V2 160 1))
    (vectorToByteString vs')
    (fromIntegral $ VS.length vs')
  return ()

genPixelRow :: (MonadIO m) => Texture -> GPUState -> m ()
genPixelRow im g = do
  let y = gpuYCoordinate $ gpuConfig g
  updateTextureLine im (fromIntegral y)
    $ backgroundLine (gpuConfig g) (gpuVideoRAM g)

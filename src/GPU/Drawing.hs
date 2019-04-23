module GPU.Drawing where

import Data.Word

import qualified Data.Vector.Storable as VS

import GPU.Memory
import GPU.GPUConfig
import GPU.Palette
import GPU.GPUState

import SDL.Video
import SDL.Vect
import Control.Monad.IO.Class

import Utilities.Vector

backgroundLine :: GPUConfig -> VideoRAM -> Word8 -> VS.Vector (V4 Word8)
backgroundLine g vram y = VS.generate 160 $ \x ->
  let x' = fromIntegral x + gpuScrollX g
      y' =              y + gpuScrollY g
      idx = loadVideoRAM' vram (backgroundTableIndex g x' y')
      c = paletteGrayscale (gpuBGPalette g) $ loadTile (tile vram (tileAddress g idx)) x' y'
  in V4 c c c 0xff

genPixelRow :: (MonadIO m) => Texture -> GPUState -> m ()
genPixelRow im g = do
  let y = gpuYCoordinate $ gpuConfig g
  let vs = VS.unsafeCast $ backgroundLine (gpuConfig g) (gpuVideoRAM g) y
  _ <- updateTexture im (Just $ fromIntegral <$> Rectangle (P $ V2 0 y) (V2 160 1))
    (vectorToByteString vs) (fromIntegral $ VS.length vs)
  return ()

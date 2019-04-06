module GPU.Drawing where

import Data.Word
import Data.Bits

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector)

import GPU.Memory
import GPU.GPUConfig
import GPU.Palette
import GPU.GPUState

import SDL.Video
import SDL.Vect
import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.Storable

backgroundLine :: GPUConfig -> VideoRAM -> Word8 -> Vector ColorCode
backgroundLine g vram y = VU.generate 160 $ \x ->
  let x' = fromIntegral x + gpuScrollX g
      y' =              y + gpuScrollY g
      idx = loadVideoRAM' vram (backgroundTableIndex g x' y')
  in loadTile (tile vram (tileAddress g idx)) x' y'

genPixelRow :: (MonadIO m) => Texture -> GPUState -> m ()
genPixelRow im g = do
  let y = gpuYCoordinate $ gpuConfig g
  (ptr', _) <- lockTexture im (Just $ fromIntegral <$> Rectangle (P $ V2 0 y) (V2 160 1))
  let ptr = castPtr ptr' :: Ptr Word8
  let aux i x = do
        liftIO $ print i
        let idx = 4 * fromIntegral i
        let c = paletteGrayscale (gpuBGPalette (gpuConfig g)) x
        liftIO $ do
          poke (ptr `plusPtr` idx)       c
          poke (ptr `plusPtr` (idx + 1)) c
          poke (ptr `plusPtr` (idx + 2)) c
          poke (ptr `plusPtr` (idx + 3)) (0xFF :: Word8)
  VU.imapM_ aux $ backgroundLine (gpuConfig g) (gpuVideoRAM g) y
  unlockTexture im

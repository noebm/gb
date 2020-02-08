{-# LANGUAGE OverloadedStrings #-}
module Graphics where

import Control.Monad.IO.Class
import SDL.Video
import SDL.Vect
import Data.Word
import Data.Text (Text)

import GPU.GPUState
import GPU.Drawing

import qualified Data.Vector.Storable as VS

import Utilities.Vector

data GraphicsContext = GraphicsContext
  { window :: Window
  , renderer :: Renderer
  , image :: Texture
  }

newWindow :: MonadIO m => Text -> V2 Int -> Maybe Int -> m GraphicsContext
newWindow wndName wndSize textureScale = do
  let wndConfig
        = maybe id (\scale x -> x { windowInitialSize = fromIntegral <$> scale *^ wndSize })
          textureScale
        $ defaultWindow
  wnd  <- createWindow wndName wndConfig
  rndr <- createRenderer wnd (negate 1) defaultRenderer
  text <- createTexture rndr ARGB8888 TextureAccessStreaming (fromIntegral <$> wndSize)
  return $ GraphicsContext wnd rndr text

initializeGraphics :: MonadIO m => m GraphicsContext
initializeGraphics = newWindow "GamerBoy" (V2 160 144) Nothing

renderImage :: MonadIO m => Renderer -> Texture -> m ()
renderImage rend text = copy rend text Nothing Nothing

renderGraphics :: MonadIO m => GraphicsContext -> m ()
renderGraphics ctx = do
  clear (renderer ctx)
  copy (renderer ctx) (image ctx) Nothing Nothing
  present (renderer ctx)

updateTextureLine :: MonadIO m => Texture -> Int -> VS.Vector (V4 Word8) -> m ()
updateTextureLine tex line vs = do
  let vs' = VS.unsafeCast vs
  let rect = Rectangle (P $ V2 0 line) (V2 (VS.length vs) 1)
  _ <- updateTexture tex
    (Just $ fromIntegral <$> rect)
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
  let v = generateLine (gpuConfig g) (gpuVideoRAM g) (gpuOAM g)
  updateTextureLine im (fromIntegral y)
    $ VS.map paletteColorToGrayscale
    $ VS.convert
    $ v

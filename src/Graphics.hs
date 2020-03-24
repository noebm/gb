{-# LANGUAGE OverloadedStrings #-}
module Graphics where

import Control.Monad.IO.Class
import Control.Monad

import SDL.Video
import SDL.Vect
import Data.Word
import Data.Text (Text)

import Hardware.GPU.Frame

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

generateImage :: MonadIO m => Texture -> Frame -> m ()
generateImage texture (Frame frame) = updateImage texture $ VS.convert frame

updateImage :: MonadIO m => Texture -> VS.Vector Word8 -> m ()
updateImage texture im = do
  when (VS.length im /= 160 * 144) $ error $
    "updateImage: input image not 160 * 144 bytes long - " ++ show (VS.length im)
  let vs' = VS.unsafeCast $ VS.map paletteColorToGrayscale im
  _ <- updateTexture texture
    Nothing
    (vectorToByteString vs')
    (4 * 160)
  return ()

{-# LANGUAGE OverloadedStrings #-}
module Graphics where

import Control.Monad.IO.Class
import SDL.Video
import SDL.Vect


data GraphicsContext = GraphicsContext
  { window :: Window
  , renderer :: Renderer
  , image :: Texture
  }

initializeGraphics :: MonadIO m => m GraphicsContext
initializeGraphics = do
  let wndName = "GamerBoy"
  let wndConfig = defaultWindow { windowInitialSize = V2 (160*2) (144*2) }
  wnd  <- createWindow wndName wndConfig
  rndr <- createRenderer wnd (negate 1) defaultRenderer
  text <- createTexture rndr ARGB8888 TextureAccessStreaming (V2 160 144)
  return $ GraphicsContext wnd rndr text

renderImage :: MonadIO m => Renderer -> Texture -> m ()
renderImage rend text = copy rend text Nothing Nothing

renderGraphics :: MonadIO m => GraphicsContext -> m ()
renderGraphics ctx = do
  clear (renderer ctx)
  copy (renderer ctx) (image ctx) Nothing Nothing
  present (renderer ctx)

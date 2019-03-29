{-# LANGUAGE OverloadedStrings #-}
module Graphics where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)

-- import qualified Data.Vector.Unboxed as V
-- import Data.Vector.Unboxed (Vector)

import Control.Monad
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
  let wndConfig = defaultWindow { windowInitialSize = V2 160 144 }
  wnd  <- createWindow wndName wndConfig
  rndr <- createRenderer wnd (negate 1) defaultRenderer
  -- surf <- getWindowSurface wnd
  surf <- getWindowSurface wnd
  text <- createTextureFromSurface rndr surf
  dim <- surfaceDimensions surf
  liftIO $ putStrLn $ "surfdim: " ++ show dim
  -- text <- createTexture rndr ARGB8888 TextureAccessStreaming (V2 160 144)
  return $ GraphicsContext wnd rndr text

renderImage :: MonadIO m => Renderer -> Texture -> m ()
renderImage rend text = copy rend text Nothing Nothing

renderGraphics :: MonadIO m => ByteString -> GraphicsContext -> m ()
renderGraphics bytes ctx = do
  -- liftIO $ putStrLn $ "writing image of size" ++ show (B.length bytes)
  when (B.length bytes > 0) $ do
    -- error "invalid imagedata"
    let rowByteCount = 160 * 4
    image' <- updateTexture (image ctx) Nothing (B.toStrict bytes) rowByteCount
    clear (renderer ctx)
    copy (renderer ctx) (image ctx) Nothing Nothing
    present (renderer ctx)
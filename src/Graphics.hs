{-# LANGUAGE OverloadedStrings #-}
module Graphics where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)

import Control.Monad
import Control.Monad.IO.Class
import SDL.Video
import SDL.Video.Renderer
import SDL.Vect

import Data.Word

import MMIO

{-
import Data.Bits
import Data.Bits.Lens
-}

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
  text <- createTexture rndr ARGB8888 TextureAccessStreaming (V2 160 144)
  return $ GraphicsContext wnd rndr text

-- initializeTexture :: MonadIO m => Renderer -> m Texture
-- initializeTexture surf = createTexture surf ARGB8888 TextureAccessStreaming (V2 160 144)

renderImage :: MonadIO m => Renderer -> Texture -> m ()
renderImage rend text = copy rend text Nothing Nothing

renderGraphics :: MonadIO m => B.ByteString -> GraphicsContext -> m ()
renderGraphics bytes ctx = do
  when (B.length bytes /= 160 * 144 * 4) $ error "invalid imagedata"
  let rowByteCount = 160 * 4
  image' <- updateTexture (image ctx) Nothing bytes rowByteCount
  clear (renderer ctx)
  copy (renderer ctx) image' Nothing Nothing
  present (renderer ctx)

-- updateLine :: Texture -> Word8 -> ByteString -> 

-- fill :: MonadIO m => Word8 -> Texture -> m ()
-- fill

-- generateImage texture = do
--   let rowByteCount = 160 * 4
--   let bytes = undefined
--   updateTexture texture Nothing bytes rowByteCount

-- data Sprite = Sprite
--   { spriteY :: Word8
--   , spriteX :: Word8
--   , spriteTile :: Word8
--   , spriteAttrib :: Word8
--   }

-- generatePixelData oam = 

-- -- | selects sprites from OAM (0xFE00 - 0xFE9F) by y address
-- selectSprites :: Vector Word8 -> Word8 -> [Vector Word8]
-- selectSprites oam y = take 10 $ filter onLine [ V.slice idx 4 oam | idx <- (*4) <$> [0..39] ]
--   where onLine sprite = sprite V.! 0 == y
-- 
-- draw (V2 sx sy) (V2 wx wy) ly usingWindow = undefined
--   where y = if usingWindow then ly - wy else sy - ly
--         tileRow = 

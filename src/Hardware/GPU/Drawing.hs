module Hardware.GPU.Drawing
  ( generateLine
  , GPUControl
  , OAM
  , VideoRAM
  ) where

import           Data.Word

import qualified Data.Vector.Unboxed           as V
import qualified Data.Vector.Unboxed.Mutable   as VM

import           Hardware.GPU.GPUControl
import           Hardware.GPU.OAM
import           Hardware.GPU.Palette
import           Hardware.GPU.VideoRAM

import           SDL.Vect

import           Control.Lens
import           Control.Monad
import           Control.Monad.Primitive

pixel :: VideoRAM -> Bool -> Bool -> V2 Word8 -> Color
pixel mem tileSelect mapSelect (V2 x y) = getTileColor t x y
  where t = getTile mapSelect tileSelect (x `div` 8) (y `div` 8) mem

backgroundLine :: GPUControl -> VideoRAM -> V.Vector Word8
backgroundLine g vram = V.generate 160 $ \i ->
  paletteValue (_gpuBGPalette g)
    $  pixel vram (g ^. gpuTileDataSelect) (g ^. gpuBGTileMapSelect)
    $  V2 (fromIntegral i) (g ^. gpuLine)
    +  g
    ^. gpuScroll

windowLine :: GPUControl -> VideoRAM -> (Word8, V.Vector Word8)
windowLine g vram =
  let y'      = g ^. gpuLine - g ^. gpuWindow . _y
      windowX = view (gpuWindow . _x) g - 7
  in  (,) windowX $ V.generate (160 - fromIntegral windowX) $ \i ->
        let x' = fromIntegral i + windowX
            x  = if x' >= windowX then fromIntegral i else x'
        in  paletteValue (_gpuBGPalette g) $ pixel
              vram
              (g ^. gpuTileDataSelect)
              (g ^. gpuWindowTileMapSelect)
              (V2 x y')

spriteLine
  :: PrimMonad m
  => GPUControl
  -> VideoRAM
  -> OAM
  -> VM.MVector (PrimState m) Word8
  -> m ()
spriteLine gctrl mem oam pixels = do
  let objsize = if gctrl ^. gpuOBJSizeLarge then 16 else 8
  V.forM_ (getLineSprites (gctrl ^. gpuLine) objsize oam) $ \obj -> do
    let pal = gctrl ^. gpuOBJPalette (obj ^. spriteDMGPalette)
    let (idx, y) =
          let y0 =
                (if obj ^. spriteFlippedY then \a -> objsize - 1 - a else id)
                  $  gctrl
                  ^. gpuLine
                  -  obj
                  ^. spritePositionY
              (idxOffset, y') = y0 `divMod` 8
          in  (obj ^. spriteTile + idxOffset, y')
    let t               = getSpriteTile idx (tiles mem)
    let adjustTileCoord = if obj ^. spriteFlippedX then (7 -) else id
    forM_ [0 .. 7] $ \x ->
      when (obj ^. spritePositionX + x < 160) $ if obj ^. spriteBGPriority
        then do
          c <- VM.read pixels (fromIntegral $ obj ^. spritePositionX + x)
          when (c == 0x00) $ do
            let objc =
                  objPaletteValue pal $ getTileColor t (adjustTileCoord x) y
            forM_ objc
              $ VM.write pixels (fromIntegral $ obj ^. spritePositionX + x)
        else do
          let objc = objPaletteValue pal $ getTileColor t (adjustTileCoord x) y
          forM_ objc
            $ VM.write pixels (fromIntegral $ obj ^. spritePositionX + x)

generateLine :: GPUControl -> VideoRAM -> OAM -> V.Vector Word8
generateLine gctrl mem oam = V.create $ do
  pixels <- VM.new 160
  when (gctrl ^. displayBG) $ do
    let bgrd = backgroundLine gctrl mem
    V.copy pixels bgrd
  when (gctrl ^. displayWindow)
    $ when (gctrl ^. gpuWindow . _y <= gctrl ^. gpuLine)
    $ do
        let (offset, disp) = windowLine gctrl mem
        V.copy (VM.drop (fromIntegral offset) pixels) disp
  when (gctrl ^. displayOBJ) $ spriteLine gctrl mem oam pixels
  return pixels

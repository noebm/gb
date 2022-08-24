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
    $ pixel vram (g ^. gpuTileDataSelect) (g ^. gpuBGTileMapSelect)
    $ V2 (fromIntegral i) (g ^. gpuLine)
    + (g ^. gpuScroll)

windowLine :: GPUControl -> VideoRAM -> (Word8, V.Vector Word8)
windowLine g vram =
  let y'      = g ^. gpuLine - g ^. gpuWindow . _y
      windowX = (g ^. gpuWindow . _x) - 7
  in  (,) windowX $ V.generate (160 - fromIntegral windowX) $ \i ->
        let x' = fromIntegral i + windowX
            x  = if x' > windowX then fromIntegral i else x'
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
    let (idx, y) =
          (gctrl ^. gpuLine - obj ^. spritePositionY)
            & (if obj ^. spriteFlippedY then (objsize - 1 -) else id)
            & (`divMod` 8)
    let visiblePixels =
          [0 .. 7]
            & map (\x -> (x, fromIntegral $ obj ^. spritePositionX + x))
            & filter (\(x, p) -> p < 160)
    let color x = objPaletteValue pal $ getTileColor tile (adjustTileCoord x) y         where
          pal             = gctrl ^. gpuOBJPalette (obj ^. spriteDMGPalette)
          tile            = getSpriteTile (obj ^. spriteTile + idx) (tiles mem)
          adjustTileCoord = if obj ^. spriteFlippedX then (7 -) else id
    forM_ visiblePixels $ \(x, pos) -> do
      let bgprio = obj ^. spriteBGPriority
      bgFlag <- if bgprio then (== 0x00) <$> VM.read pixels pos else pure True
      forM_ (guard bgFlag *> color x) $ VM.write pixels pos

generateLine :: GPUControl -> VideoRAM -> OAM -> V.Vector Word8
generateLine gctrl mem oam = V.create $ do
  pixels <- VM.new 160
  when (gctrl ^. displayBG) $ do
    let bgrd = backgroundLine gctrl mem
    V.copy pixels bgrd
  let windowVisible = gctrl ^. gpuWindow . _y <= gctrl ^. gpuLine
  when (gctrl ^. displayWindow && windowVisible) $ do
    let (offset, disp) = windowLine gctrl mem
    V.copy (VM.drop (fromIntegral offset) pixels) disp
  when (gctrl ^. displayOBJ) $ spriteLine gctrl mem oam pixels
  return pixels

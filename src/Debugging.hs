module Debugging where

import Control.Monad.IO.Class
import Control.Monad

import Data.Word
import Data.Bits
import Data.Maybe

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Storable.Mutable as VS

import SDL.Video
import SDL.Vect (V2(..))

import MonadEmulator
import Drawing
import Memory.MMIO

clearRegisters :: MonadEmulator m => m ()
clearRegisters = do
  forM_ [A,F,B,C,D,E,H,L] $ \r -> store8 (Register8 r) 0x00
  forM_ [PC,SP] $ \r -> store16 (Register16 r) 0x0000

testRegisters :: (MonadEmulator m, MonadIO m) => m ()
testRegisters = do
  forM_ [A,F,B,C,D,E,H,L] $ \r -> do
    store8 (Register8 r) 0xFF
    liftIO . putStrLn =<< showRegisters
    store8 (Register8 r) 0x00
  forM_ [AF,BC,DE,HL,PC,SP] $ \r -> do
    store16 (Register16 r) 0xFF00
    liftIO . putStrLn =<< showRegisters
    store16 (Register16 r) 0x0000

-- dumpVRAM :: V.MVector s Word8 -> IO ()
dumpVRAM mem = liftIO $ do
  x <- forM [0x8000..0x9FFF] $ V.read mem
  print x

getBackgroundMap :: MonadEmulator m => LCDConfig -> m [ [ Word8 ] ]
getBackgroundMap conf = fmap (filter (/= [])) . forM [0..255] $ \y ->
  fmap (filter (/= 0)) . forM [0..255] $ \x -> getBackgroundTileIndex conf y x

drawCompleteBackground :: (MonadIO m, MonadEmulator m) => m Surface -- B.ByteString
drawCompleteBackground = do
  -- make sure lcdConfig is true
  lcd <- fromJust <$> do
    cfg <- load8 control
    store8 control (cfg .|. 0x80)
    lcdConfig <* store8 control cfg
  pal <- bgPalette
  ar <- liftIO $ VS.new $ 256 * 256 * 3
  -- bs <- fromJust <$> forM lcd $ \cfg -> do
  forM_ [0..255] $ \y ->
    forM_ [0..255] $ \x -> do
      idx <- load8 $ backgroundTileIndex lcd y x
      c <- pal <$> tile (tileAddr lcd idx) y x
      let i = (fromIntegral y * 256 + fromIntegral x) * 3
      liftIO $ do
        VS.unsafeWrite ar (i + 0) c
        VS.unsafeWrite ar (i + 1) c
        VS.unsafeWrite ar (i + 2) c
  createRGBSurfaceFrom ar (V2 256 256) (256 * 3) RGB888


getTileData :: MonadEmulator m => LCDConfig -> Word8 -> m [Word16]
getTileData conf idx = do
  let addr = tileAddr conf idx
  forM [0,2..8*2 - 1] $ \k ->
    load16 (Addr16 $ addr + k)

drawTile :: (MonadIO m, MonadEmulator m) => LCDConfig -> Word8 -> m Surface
drawTile conf idx = do
  let addr = tileAddr conf idx
  let sx = 8
  let sy = 8
  let depth = 3
  ar <- liftIO $ VS.new $ fromIntegral sx * fromIntegral sy * depth
  bgrdPal <- paletteValue <$> load8 backgroundPalette
  forM_ [0..sy - 1 :: Word8] $ \y ->
    forM_ [0..sx - 1 :: Word8] $ \x -> do
      colorNumber <- bgrdPal <$> tile addr y x
      -- let i = fromIntegral $ (y * sx + x) * depth
      let i = (fromIntegral y * fromIntegral sx + fromIntegral x) * depth
      -- liftIO $ do
      --   putStrLn $ printf "x: %d y: %d i: %03d" x y i
      liftIO $ case colorNumber of
        0 -> do
          VS.unsafeWrite ar (i + 0) 0
          VS.unsafeWrite ar (i + 1) 0
          VS.unsafeWrite ar (i + 2) 0
        1 -> do
          VS.unsafeWrite ar (i + 0) 0xFF
          VS.unsafeWrite ar (i + 1) 0
          VS.unsafeWrite ar (i + 2) 0
        2 -> do
          VS.unsafeWrite ar (i + 0) 0
          VS.unsafeWrite ar (i + 1) 0xFF
          VS.unsafeWrite ar (i + 2) 0
        3 -> do
          VS.unsafeWrite ar (i + 0) 0
          VS.unsafeWrite ar (i + 1) 0
          VS.unsafeWrite ar (i + 2) 0xFF
        _ -> error "impossible"
      -- liftIO $ VS.unsafeWrite ar (i + 3) 0xFF
  createRGBSurfaceFrom ar (fromIntegral <$> V2 sx sy) (fromIntegral sx * fromIntegral depth - 1) RGB888

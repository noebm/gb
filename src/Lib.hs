{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Lib
where

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Storable.Mutable as VS

import Data.Foldable
import Data.Word
import Data.Bits

import Text.Printf

import SDL.Video hiding (paletteColor)
import SDL.Vect hiding (rotate)

import Control.Monad.IO.Class
import Control.Monad

import Graphics
import MonadEmulator
import GB
import Instruction
import Cartridge
import Memory.MMIO

import Debugging
import Drawing

interpret :: (MonadEmulator m, MonadIO m) => Bool -> GraphicsContext -> m Bool
interpret enablePrinting gfx = do
  regs <- showRegisters
  b <- immediate8

  when enablePrinting $ do
    -- liftIO $ putStrLn $ printf "Instruction: 0x%02x / PC: 0x%04x" b pc
    liftIO $ putStrLn regs
    liftIO $ putStrLn $ printf "Instruction: 0x%02x" b

  advCycles =<< instruction b

  lcd <- lcdConfig
  forM_ lcd $ \conf -> do
    gpuInstr <- updateGPU
    forM_ gpuInstr $ \case
      DrawLine  -> genPixelRow (image gfx) conf
      DrawImage -> renderGraphics gfx
  stop

disableBootRom :: MonadEmulator m => m Bool
disableBootRom = (`testBit` 0) <$> load8 (Addr8 0xFF50)

-- not quite correct (only works for games without mbc)
writeCartridge cart = copyData (cartridgeData cart)

copyData bs = do
  mem <- unsafeMemory
  liftIO $ forM_ [0..B.length bs - 1] $ \idx ->
    V.write mem idx (bs `B.index` idx)

drawTile :: (MonadIO m, MonadEmulator m) => LCDConfig -> Word8 -> m Surface
drawTile conf idx = do
  let addr = tileAddr conf idx
  ar <- liftIO $ VS.new $ 8 * 8 * 3
  bgrdPal <- paletteValue <$> load8 backgroundPalette
  forM_ [0..7 :: Word8] $ \y ->
    forM_ [0..7 :: Word8] $ \x -> do
      colorNumber <- tile addr y x
      let i = fromIntegral $ (y * 8 + x) * 3
      liftIO $ case bgrdPal colorNumber of
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
  createRGBSurfaceFrom ar (V2 8 8) (8 * 3) RGB888

someFunc :: IO ()
someFunc = do
  rom <- memoryBootRom
  cartOrError <- loadCartridge "./Tetris.gb"
  -- cartOrError <- loadCartridge "./testroms/cpu_instrs/individual/03-op sp,hl.gb"
  let cart = either error id cartOrError
  runGB $ do
    -- copy boot rom to memory
    writeCartridge cart
    copyData rom

    let g fx = do
          liftIO . print =<< load16 (Register16 PC)
          s <- interpret False fx
          unless s $ g fx
    let f fx = do
            s <- interpret False fx
            unless s $ do
              bootflag <- disableBootRom
              if bootflag
                then do
                -- mapM_ (liftIO . print <=< getBackgroundMap) =<< lcdConfig
                -- surf <- drawCompleteBackground

                -- c <- lcdConfig
                -- forM_ c $ \conf -> do
                --   forM_ [0..25] $ \k -> do
                --   -- let k = 25
                --     surf <- drawTile conf k
                --     text <- createTextureFromSurface (renderer fx) surf
                --     renderGraphics (fx { image = text })
                --     void $ liftIO $ getLine

                writeCartridge cart >> g fx
                else f fx
    gfx <- initializeGraphics
    f gfx

    -- liftIO $ 

    -- surf <- drawCompleteBackground
    -- text <- createTextureFromSurface (renderer gfx) surf
    -- renderGraphics (gfx { image = text })
    liftIO $ do
      putStrLn "waiting for input"
      void getLine


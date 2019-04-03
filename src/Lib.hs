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
import BootRom
import Memory.MMIO

import Interrupt
import Debugging
import Drawing

import Instruction.Interpret
import Instruction.Instruction (parseInstructionM, Instruction, Arg)
import Instruction.Disassembler

update :: (MonadEmulator m, MonadIO m) => GraphicsContext -> m Bool
update gfx = do
  -- mapM_ (advCycles <=< enterInterrupt) =<< handleInterrupt

  if False
    then do
    i <- parseInstructionM byte
    interpretM i
    advCycles 4
    else
    advCycles =<< Instruction.instruction =<< byte

  lcd <- lcdConfig
  forM_ lcd $ \conf -> do
    gpuInstr <- updateGPU
    forM_ gpuInstr $ \case
      DrawLine  -> genPixelRow (image gfx) conf
      DrawImage -> renderGraphics gfx

  stop
  -- (,) i <$> stop

interpret :: (MonadEmulator m, MonadIO m) => Bool -> GraphicsContext -> m Bool
interpret enablePrinting gfx = do
  pc <- load16 (Register16 PC)
  s <- update gfx
  return s

disableBootRom :: MonadEmulator m => m Bool
disableBootRom = (`testBit` 0) <$> load8 (Addr8 0xFF50)

-- not quite correct (only works for games without mbc)
writeCartridge cart = copyData (cartridgeData cart)

copyData bs = do
  mem <- unsafeMemory
  liftIO $ forM_ [0..B.length bs - 1] $ \idx ->
    V.write mem idx (bs `B.index` idx)

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

someFunc :: Maybe FilePath -> IO ()
someFunc fp' = do
  rom <- readBootRom
  cart <- maybe (return emptyCartridge)
    (\fp -> do
        cartOrError <- loadCartridge fp
        return $ either error id cartOrError) fp'
  runGB cart $ do
    loadBootRom rom

    -- liftIO . mapM_ print =<< runDisassembler (\addr -> addr >= 0x8000)

    let runTillStop fx = do
          s <- interpret False fx
          unless s $ runTillStop fx

    let bootStrap fx = do
          s <- interpret False fx
          bootflag <- disableBootRom
          unless (s || bootflag) $ bootStrap fx

    gfx <- initializeGraphics
    bootStrap gfx
    unloadBootRom (cartridgeData cart)
    runTillStop gfx

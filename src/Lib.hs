{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
where

import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed.Mutable as V

import Data.List (transpose)
import Data.Foldable
import Data.Traversable
import Data.Word
import Data.Int
import Data.Bits

import Text.Printf

import SDL.Video hiding (paletteColor)
import SDL.Vect
import Foreign.Ptr
import Foreign.Storable

import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe

import Graphics
import MonadEmulator
import GB
import Instruction
import Cartridge
import Memory.MMIO
import Memory.OAM


interpret :: (MonadEmulator m, MonadIO m) => Bool -> GraphicsContext -> m ()
interpret enablePrinting gfx = do
  -- pc <- load16 (Register16 PC)
  -- when (pc == 0xe9) $ return () -- error "at 0xe9"
  -- when (pc >  0xff) $ error "something happened"

  regs <- showRegisters
  b <- immediate8
  when enablePrinting $ do
    -- liftIO $ putStrLn $ printf "Instruction: 0x%02x / PC: 0x%04x" b pc
    liftIO $ putStrLn regs
    liftIO $ putStrLn $ printf "Instruction: 0x%02x" b

  -- when (pc > 0x0b) $ void $ liftIO getLine
  advCycles =<< instruction b
  lcd <- lcdConfig

  if isJust lcd then do
    gpuInstr <- updateGPU
    case gpuInstr of
      Just DrawLine -> genPixelRow (image gfx)
      Just DrawImage -> renderGraphics gfx
      _ -> return ()
  else return ()

disableBootRom :: MonadEmulator m => m Bool
disableBootRom = (`testBit` 0) <$> load8 (Addr8 0xFF50)

-- not quite correct (only works for games without mbc)
writeCartridge cart = do
  mem <- unsafeMemory
  liftIO $ forM_ [0..B.length (cartridgeData cart) - 1] $ \idx ->
    V.write mem idx (cartridgeData cart `B.index` idx)

someFunc :: IO ()
someFunc = do
  rom <- memoryBootRom
  Just cart <- loadCartridge "./Tetris.gb"
  runGB $ do
    writeCartridge cart
    -- copy boot rom to memory
    mem <- unsafeMemory
    liftIO $ forM_ [0..B.length rom - 1] $ \idx ->
      V.write mem idx (rom `B.index` idx)

    let g fx = interpret False fx >> g fx
    let f fx = do
          -- pc <- load16 (Register16 PC)
          -- unless (pc == 0x62) $ do
            interpret False fx
            bootflag <- disableBootRom
            when bootflag (writeCartridge cart >> g fx)
            f fx
    gfx <- initializeGraphics
    f gfx


paletteColor :: Word8 -> Word8 -> Word8
paletteColor pal sel =
  case (pal `shiftR` fromIntegral (2 * sel)) .&. 3 of
    0 -> 255
    1 -> 192
    2 -> 96
    3 -> 0
    _ -> error "impossible"

-- | Given the address of the tile and x / y pixel coordinates,
-- find the palette index of the pixel.
tile :: MonadEmulator m => Word16 -> (Word8 -> Word8 -> m Word8)
tile tileAddress y x = do
  let yOffset = fromIntegral (y .&. 7) * 2 -- every line contains 2 bytes
  b0 <- load8 (Addr8 $ tileAddress + yOffset)
  b1 <- load8 (Addr8 $ tileAddress + yOffset + 1)
  let xOffset = fromIntegral (x .&. 7)
  let paletteSelect = fromIntegral $ 2 * fromEnum (b1 `testBit` xOffset) .|. fromEnum (b0 `testBit` xOffset)
  return paletteSelect

bgPalette :: MonadEmulator m => m (Word8 -> Word8)
bgPalette = do
  pal <- load8 backgroundPalette
  return $ \w -> paletteColor pal w

drawLineBackground :: (MonadIO m, MonadEmulator m) => m (Word8 -> Word8 -> m Word8)
drawLineBackground = do
  lcdconf <- lcdConfig
  case lcdconf of
    Just lcd -> do
      bgrdPal <- bgPalette
      sy <- load8 scrollY
      sx <- load8 scrollX
      return $ \y x -> do
        let y' = sy + y
        let x' = sx + x
        idx <- load8 $ backgroundTileIndex lcd y' x'
        bgrdPal <$> tile (tileAddr lcd idx) y' x'
    Nothing -> return $ \_ _ -> undefined

genPixelRow :: (MonadIO m, MonadEmulator m) => Texture -> m ()
genPixelRow im = do
  fBgrd <- drawLineBackground
  y <- load8 currentLine
  (ptr', _) <- lockTexture im (Just $ fmap fromIntegral $ Rectangle (P $ V2 0 y) (V2 160 1))
  let ptr = castPtr ptr' :: Ptr Word8
  forM_ [0..159] $ \x -> do
    c <- fBgrd y x
    let idx = 4 * fromIntegral x
    liftIO $ do
      poke (ptr `plusPtr` idx)       c
      poke (ptr `plusPtr` (idx + 1)) c
      poke (ptr `plusPtr` (idx + 2)) c
      poke (ptr `plusPtr` (idx + 3)) (255 :: Word8)
  unlockTexture im

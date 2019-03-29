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


interpret :: (MonadEmulator m, MonadIO m) => Bool -> GraphicsContext -> Build.Builder -> m Build.Builder
interpret enablePrinting gfx bs = do
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
      Nothing -> return bs
      Just DrawLine -> do
        dbytes <- genPixelRow'
        return $ bs <> dbytes
      Just DrawImage -> do
        let bytes = Build.toLazyByteString bs
        renderGraphics bytes gfx
        -- genPixelRow
        return mempty
    else return bs

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

    gfx <- initializeGraphics
    let g b = g =<< interpret False gfx b
    let f b = do
          -- pc <- load16 (Register16 PC)
          -- unless (pc == 0x62) $ do
            b' <- interpret False gfx b
            bootflag <- disableBootRom
            when bootflag (writeCartridge cart >> g b')
            f b'
    f mempty


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
tile tileAddr y x = do
  let yOffset = fromIntegral (y .&. 7) * 2 -- every line contains 2 bytes
  b0 <- load8 (Addr8 $ tileAddr + yOffset)
  b1 <- load8 (Addr8 $ tileAddr + yOffset + 1)
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
  bgrdPal <- bgPalette
  case lcdconf of
    Just lcd -> return $ \y x -> do
      sy <- load8 scrollY
      sx <- load8 scrollX
      let y' = sy + y
      let x' = sx + x
      idx <- load8 $ backgroundTileIndex lcd y' x'
      bgrdPal <$> tile (tileAddr lcd idx) y' x'
    Nothing -> return $ \_ _ -> undefined

genPixelRow' :: (MonadIO m, MonadEmulator m) => m Build.Builder
genPixelRow' = do
  fBgrd <- drawLineBackground
  y <- load8 currentLine
  fmap mconcat $ for [0..159] $ \x -> do
    c <- fBgrd y x
    return $ mconcat $ fmap Build.word8 [c,c,c,255]

{-
genPixelRow :: MonadEmulator m => m Build.Builder
genPixelRow = do
  sx <- load8 scrollX
  sy <- load8 scrollY
  winx <- (\x -> x - 7) <$> load8 windowX
  winy <- load8 windowY

  winEnabled <- (`testBit` 5) <$> load8 control
  -- winEnabled <- use $ memory.mmio.lcdc.bitAt 5
  line <- load8 currentLine -- use $ memory.mmio.ly
  let useWindow = winEnabled && winy <= line
  bgAddrBase <- do
    objTile     <- (`testBit` 3) <$> load8 control
    windowTile  <- (`testBit` 6) <$> load8 control
    return $ if (useWindow && windowTile) || (not useWindow && objTile)
      then 0x9C00
      else 0x9800
  tileSelect <- (`testBit` 4) <$> load8 control

  let y = if useWindow then line - winy else sy - line
  let tileRow = fromIntegral (y `div` 8) * 320

  fmap mconcat $ for [0..159] $ \pixel -> do
    let x = if useWindow && pixel >= winx then pixel - winx else pixel + sx
    let tileCol = fromIntegral x `div` 8
    let tileAddr = Addr8 $ bgAddrBase + tileRow + tileCol

    tileLoc <- tileLocation tileSelect <$> load8 tileAddr
      -- uses (memory . videoRAM . singular (ix $ fromIntegral $ tileAddr .&. 0x1FFF)) (tileLocation tileMode)
    let line' = fromIntegral $ (y .&. 0x7) * 2
    b0 <- load8 (Addr8 $ tileLoc + line')
    b1 <- load8 (Addr8 $ tileLoc + line' + 1)
    let colourNumber = 2 * fromEnum ((b1 `testBit` colour) `shiftL` 1) + fromEnum (b0 `testBit` colour) -- :: Int
          where colour = negate (fromIntegral (x `mod` 8) - 7) :: Int
    -- c <- uses (memory.mmio.mmioData.singular (ix 0x47)) (\pal -> colour' pal colourNumber)
    c <- (\pal -> paletteColor pal colourNumber) <$> load8 backgroundPalette
    return $ mconcat $ fmap Build.word8 [255,c,c,c]
  -- return ()
-}

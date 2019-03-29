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

import Graphics
import MonadEmulator
import GB
import Instruction
import Cartridge
import Memory.MMIO
import Memory.OAM

interpret :: (MonadEmulator m, MonadIO m) => Bool -> GraphicsContext -> Build.Builder -> m Build.Builder
interpret enablePrinting gfx bs = do
  pc <- load16 (Register16 PC)
  when (pc == 0xe9) $ return () -- error "at 0xe9"
  when (pc >  0xff) $ error "something happened"

  regs <- showRegisters
  b <- immediate8
  when enablePrinting $ do
    -- liftIO $ putStrLn $ printf "Instruction: 0x%02x / PC: 0x%04x" b pc
    liftIO $ putStrLn regs
    liftIO $ putStrLn $ printf "Instruction: 0x%02x" b

  -- when (pc > 0x0b) $ void $ liftIO getLine
  advCycles =<< instruction b

  lcd <- lcdEnable
  if lcd then do
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

  -- interpret enablePrinting t' bytes'
  -- interpret enablePrinting gfx bs'

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
    -- liftIO $ do
    --   print $ rom `B.index` 0x62
    --   print =<< V.read mem 0x62

    gfx <- initializeGraphics
    let g b = g =<< interpret True gfx b
    let f b = do
          -- pc <- load16 (Register16 PC)
          -- unless (pc == 0x62) $ do
            b' <- interpret False gfx b
            bootflag <- disableBootRom
            when bootflag (writeCartridge cart >> g b')
            f b'
    f mempty

-- dumpVRAM mem = liftIO $ do
--   x <- forM [0x8000..0x9FFF] $ V.read mem
--   print x

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

colour' :: Word8 -> Int -> Word8
colour' palette sel =
  case (palette `shiftR` (2 * (4 - sel))) .&. 3 of
    0 -> 255
    1 -> 192
    2 -> 96
    3 -> 0
    _ -> error "impossible"

paletteColor :: Word8 -> Word8 -> Word8
paletteColor pal sel =
  case (pal `shiftR` fromIntegral (2 * (4 - sel))) .&. 3 of
    0 -> 255
    1 -> 192
    2 -> 96
    3 -> 0
    _ -> error "impossible"

-- location from start of videoRAM
tileLocation :: Bool -> Word8 -> Word16
tileLocation True  w = 0x8000 + 16 * (fromIntegral w + 128)
tileLocation False w = fromIntegral $ 0x8800 + 16 * (fromIntegral w :: Int32)

-- getTile :: Word16 -> 

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
  -- backgroundEnabled <- backgroundDisplay
  -- when backgroundEnabled $ do
    sx <- load8 scrollX
    sy <- load8 scrollY
    liftIO $ putStrLn $ printf "sx: %02x sy: %02x" sx sy
    -- let tileRowSize = 8
    -- let yScale = 144 `div` tileSize -- 9
    -- let bgrdScrollOffset = fromIntegral sx `div` tileRowSize + fromIntegral sy * yScale

    bgrdBaseTableBase <- backgroundTileTableAddrBase
    tileAddrMode      <- tilePatternAddr
    bgrdPal           <- bgPalette
    return $ \y x -> do
      let y' = y + sy
      let x' = x + sx
      let bgrdTableIndex = fromIntegral (x' `div` 8) + 32 * fromIntegral (y' `div` 8)
      tileIndex <- load8 $ Addr8 $ bgrdBaseTableBase + bgrdTableIndex
      -- liftIO $ putStrLn $ printf "x: %02x y: %02x table: %04x index: %02x" x' y' (bgrdBaseTableBase + bgrdTableIndex) tileIndex
      bgrdPal <$> tile (tileAddrMode tileIndex) y x

genPixelRow' :: (MonadIO m, MonadEmulator m) => m Build.Builder
genPixelRow' = do
  fBgrd <- drawLineBackground
  y <- load8 currentLine
  fmap mconcat $ for [0..159] $ \x -> do
    c <- fBgrd y x
    return $ mconcat $ fmap Build.word8 [255,c,c,c]

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
    let colourNumber = 2 * fromEnum ((b1 `testBit` colour) `shiftL` 1) + fromEnum (b0 `testBit` colour) :: Int
          where colour = negate (fromIntegral (x `mod` 8) - 7) :: Int
    -- c <- uses (memory.mmio.mmioData.singular (ix 0x47)) (\pal -> colour' pal colourNumber)
    c <- (\pal -> colour' pal colourNumber) <$> load8 backgroundPalette
    return $ mconcat $ fmap Build.word8 [255,c,c,c]
  -- return ()

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
where

import Control.Lens
import Control.Monad.State
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Build

import Data.Traversable

import SDL.Video.Renderer
import Data.Int
import Data.Word
import Data.Bits
import Data.Bits.Lens

import MonadEmulator
import Instruction

import CPUState
import GBState
import Memory (memoryBootRom, mmio, videoRAM)
import MMIO
import Graphics (renderGraphics)

newtype GBT m a = GBT (StateT GBState m a)
  deriving (Functor, Applicative, Monad, MonadState GBState, MonadIO)

runGB :: Monad m => GBT m a -> GBState -> m a
runGB (GBT act) = evalStateT act

instance Monad m => MonadEmulator (GBT m) where
  store8 (Register8 r) = assign (cpuState.reg8lens r)
  store8 (Addr8 addr)  = writeMemory addr

  load8 (Register8 r) = use (cpuState.reg8lens r)
  load8 (Addr8 addr)  = accessMemory addr

  store16 (Register16 r) dw = assign (cpuState.reg16lens r) dw
  store16 (Addr16 addr) dw = do
    let (hw , lw) = dw ^. from word16
    store8 (Addr8 addr) lw
    store8 (Addr8 $ addr + 1) hw

  load16 (Register16 r) = use (cpuState.reg16lens r)
  load16 (Addr16 addr) = do
    lw <- load8 (Addr8 addr)
    hw <- load8 (Addr8 $ addr + 1)
    return $ (hw , lw) ^. word16

  advCycles dt = do
    timer += dt
    -- reset timer after some reasonable amount of time
    let pow24 = 16777216 -- 2 ** 24
    timer %= \t -> if t >= pow24 then t - pow24 else t
  getCycles = use timer

colour' :: Word8 -> Int -> Word8
colour' palette sel =
  case (palette `shiftR` (2 * sel)) .&. 3 of
    0 -> 255
    1 -> 192
    2 -> 96
    3 -> 0
    _ -> error "impossible"

-- location from start of videoRAM
tileLocation :: Bool -> Word8 -> Word16
tileLocation True  w = 0x00 + 16 * (fromIntegral w + 128)
tileLocation False w = fromIntegral $ 0x800 + 16 * (fromIntegral w :: Int16)

-- getTile :: Word16 -> 

genPixelRow :: MonadIO m => GBT m B.ByteString
genPixelRow = do
  sx <- use $ memory.mmio.scx
  sy <- use $ memory.mmio.scy
  winx <- uses (memory.mmio.wx) (\x -> x - 7)
  winy <- use $ memory.mmio.wy

  winEnabled <- use $ memory.mmio.lcdc.bitAt 5
  line <- use $ memory.mmio.ly
  let useWindow = winEnabled && winy <= line
  bgAddrBase <- do
    a <- use $ memory.mmio.lcdc.bitAt 3
    b <- use $ memory.mmio.lcdc.bitAt 6
    return $ if (useWindow && b) || (not useWindow && a)
      then 0x9C00
      else 0x9800 -- :: Word16
  tileMode <- use $ memory.mmio.lcdc.bitAt 4

  let y = if useWindow then line - winy else sy - line
  let tileRow = fromIntegral (y `div` 8) * 32

  fmap (B.pack . concat) $ for [0..159] $ \pixel -> do
    let x = if useWindow && pixel >= winx then pixel - winx else pixel + sx
    let tileCol = fromIntegral x `div` 8
    let tileAddr = bgAddrBase + tileRow + tileCol :: Word16

    tileLoc <- uses (memory . videoRAM . singular (ix $ fromIntegral $ tileAddr .&. 0x1FFF)) (tileLocation tileMode)
    let line' = fromIntegral $ (y .&. 0x7) * 2
    b0 <- use $ memory.videoRAM . singular (ix $ fromIntegral $ (tileLoc + line') .&. 0x1FFF)
    b1 <- use $ memory.videoRAM . singular (ix $ fromIntegral $ (tileLoc + line' + 1) .&. 0x1FFF)
    let colourNumber = 2 * fromEnum ((b1 ^. bitAt colour) `shiftL` 1) + fromEnum (b0 ^. bitAt colour) :: Int
          where colour = fromIntegral $ - (fromIntegral (x `mod` 8) - 7) :: Int
    c <- uses (memory.mmio.mmioData.singular (ix 0x47)) (\pal -> colour' pal colourNumber)
    return [255,c,c,c]
  -- return ()

interpret :: MonadIO m => Bool -> Word -> Build.Builder -> GBT m ()
interpret enablePrinting t bs = do
  b <- immediate8
  advCycles =<< instruction b

  t' <- getCycles
  let dt = t' - t

  m <- use $ memory.mmio
  (f , m') <- (`runStateT` m) $ do
    updateGPU dt
  assign (memory.mmio) m'

  bytes' <- case f of
    Nothing -> return bs
    Just DrawLine -> do
      dbytes <- genPixelRow
      return $ bs <> Build.byteString dbytes
    Just DrawImage -> do
      let bytes = Build.toLazyByteString bs
      renderGraphics bytes =<< use graphics
      return mempty

  when enablePrinting $ do
    pc <- use $ cpuState.regPC
    if pc == 0xe9 then error "at 0xe9" else return ()
    -- liftIO $ putStrLn $ printf "Instruction: 0x%02x / PC: 0x%04x" b pc
    -- l <- use $ memory.mmio.ly
    -- liftIO $ putStrLn $ printf "Linenumber %d" l
    -- ly <- use memory.mmio.ly
    -- liftIO . print =<< use cpuState
  interpret enablePrinting t' bytes'

someFunc :: IO ()
someFunc = do
  rom <- memoryBootRom
  s <- newGBState rom
  let bs = B.replicate (160 * 144 * 4) (0x88)
  (`runGB` s) $ do
    memory.mmio.statMode .= HBlank
    interpret True 0 mempty
  return ()

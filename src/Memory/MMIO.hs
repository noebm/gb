{-# LANGUAGE PatternSynonyms #-}
module Memory.MMIO where

import Control.Monad
import Data.Foldable
import Data.Bits
import Data.Word

import MonadEmulator

-- status mode
pattern HBlank = 0
pattern VBlank = 1
pattern OAM    = 2
pattern VRAM   = 3

updateGPU' :: Word -> Word8 -> Word8 -> (Word, Word8, Maybe Word8)
updateGPU' t l mode = case mode .&. 0x3 of
  HBlank -> update 204 (\t' -> (t' , l+1, Just $ if l == 143 then VBlank else OAM))
  VBlank -> update 456 (\t' -> if l > 153 then (t' , 0, Just OAM) else (t', l + 1, Nothing))
  OAM    -> update 80  (\t' -> (t' , l, Just VRAM))
  VRAM   -> update 172 (\t' -> (t' , l, Just HBlank))
  _ -> error "impossible"
  where update clocktime f = if t >= clocktime then f (t - clocktime) else (t, l, Nothing)

data DrawInstruction = DrawLine | DrawImage

control           = Addr8 0xFF40
status            = Addr8 0xFF41
scrollY           = Addr8 0xFF42
scrollX           = Addr8 0xFF43
currentLine       = Addr8 0xFF44
compareLine       = Addr8 0xFF45
backgroundPalette = Addr8 0xFF47
spritePalette0    = Addr8 0xFF48
spritePalette1    = Addr8 0xFF49
windowY           = Addr8 0xFF4A
windowX           = Addr8 0xFF4B

updateGPU :: MonadEmulator m => m (Maybe DrawInstruction)
updateGPU = do
  -- t <- dotClock <+= dt
  t <- resetCycles
  stat <- load8 status
  let mode = 3 .&. stat
  l <- load8 currentLine
  let (t', l', mode') = updateGPU' t l mode
  advCycles t'
  store8 currentLine l'
  for_ mode' $ \m -> store8 status (m .|. (stat .&. 0xFC))
  return $ case True of
    _ | mode' == Just HBlank && mode == VRAM   -> Just DrawLine
      -- {}| mode' == Just OAM    && mode == VBlank -> Just DrawLine
      | mode' == Just VBlank && mode == HBlank -> Just DrawImage
      | otherwise -> Nothing
{-
Bit7  LCD operation                           | ON            | OFF
Bit6  Window Tile Table address               | 9C00-9FFF     | 9800-9BFF
Bit5  Window display                          | ON            | OFF
Bit4  Tile Pattern Table address              | 8000-8FFF     | 8800-97FF
Bit3  Background Tile Table address           | 9C00-9FFF     | 9800-9BFF
Bit2  Sprite size                             | 8x16          | 8x8
Bit1  Color #0 transparency in the window     | SOLID         | TRANSPARENT
Bit0  Background display
-}
lcdEnable :: MonadEmulator m => m Bool
lcdEnable = (`testBit` 7) <$> load8 control

windowTileTableAddBase :: MonadEmulator m => m Word16
windowTileTableAddBase = do
  wndMode <- (`testBit` 6) <$> load8 control
  return $ if wndMode then 0x9C00 else 0x9800

windowDisplay :: MonadEmulator m => m Bool
windowDisplay = (`testBit` 5) <$> load8 control

backgroundTileTableAddrBase :: MonadEmulator m => m Word16
backgroundTileTableAddrBase = do
  bgMode <- (`testBit` 3) <$> load8 control
  return $ if bgMode then 0x9C00 else 0x9800

backgroundDisplay :: MonadEmulator m => m Bool
backgroundDisplay = (`testBit` 0) <$> load8 control

tilePatternTableAddrBase :: MonadEmulator m => m Word16
tilePatternTableAddrBase = do
  patternMode <- (`testBit` 4) <$> load8 control
  return $ if patternMode then 0x8000 else 0x8800

tilePatternAddr :: MonadEmulator m => m (Word8 -> Word16)
tilePatternAddr = do
  patternMode <- (`testBit` 4) <$> load8 control
  let tileSize = 16
  return $ if patternMode
    then \w -> 0x8000 + tileSize * fromIntegral w
    else \w -> 0x8800 + tileSize * (fromIntegral w + 128)

-- sprite display?
-- colorZeroTransparent :: MonadEmulator m => m Bool
-- colorZeroTransparent = (`testBit` 1) <$> load8 control

spriteSize :: MonadEmulator m => m Word8
spriteSize = do
  spriteMode <- (`testBit` 2) <$> load8 control
  return $ if spriteMode then 16 else 8

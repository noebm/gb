{-# LANGUAGE PatternSynonyms #-}
module Memory.MMIO where

import Control.Monad
import Data.Bits
import Data.Word

import MonadEmulator hiding (updateGPU)

-- status mode
pattern HBlank = 0
pattern VBlank = 1
pattern OAM    = 2
pattern VRAM   = 3

{-# INLINE updateGPU' #-}
updateGPU' :: Word -> Word8 -> Word8 -> (Word, Word8, Maybe Word8)
updateGPU' t l mode = case mode .&. 0x3 of
  HBlank -> update 204 (\t' -> (t' , l+1, Just $ if l == 143 then VBlank else OAM))
  VBlank -> update 456 (\t' -> if l > 153 then (t' , 0, Just OAM) else (t', l + 1, Nothing))
  OAM    -> update 80  (\t' -> (t' , l, Just VRAM))
  VRAM   -> update 172 (\t' -> (t' , l, Just HBlank))
  _ -> error "impossible"
  where
    {-# INLINE update #-}
    update clocktime f = if t >= clocktime then f (t - clocktime) else (t, l, Nothing)

interruptFlag   = Addr8 0xFF0F
interruptEnable = Addr8 0xFFFF
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

bootRomEnabled :: MonadEmulator m => m Bool
bootRomEnabled = not . (`testBit` 0) <$> load8 (Addr8 0xFF50)

updateGPU :: MonadEmulator m => m (Maybe Word8)
updateGPU = do
  t <- resetCycles
  stat <- load8 status
  let mode = 3 .&. stat
  l <- load8 currentLine
  let (t', l', mode') = updateGPU' t l mode
  advCycles t'
  store8 currentLine l'

  lyC <- load8 compareLine
  let lyF = l' == lyC
  let stat' = (stat `clearBit` 2) .|. if lyF then 0x04 else 0x00

  case mode' of
    Just m  -> store8 status (m .|. (stat' .&. 0xF8))
    Nothing -> store8 status stat'

  let lyIF     = lyF && stat `testBit` 6
  let oamIF    = mode' == Just OAM    && stat `testBit` 5
  let vblankIF = mode' == Just VBlank && stat `testBit` 4
  let hblankIF = mode' == Just HBlank && stat `testBit` 3

  -- let updateI = lyIF
  --       || isJust (mode' >>= \m ->
  --                   guard (not $ m `testBit` 0) >> -- not VRAM or VBlank
  --                   guard (stat `testBit` (3 + fromIntegral m)))

  ie <- load8 interruptEnable
  when (any id [ lyIF, oamIF, hblankIF ] && ie `testBit` 1) $
    store8 interruptFlag . (`setBit` 1) =<< load8 interruptFlag

  when (vblankIF && ie `testBit` 0) $
    store8 interruptFlag . (`setBit` 0) =<< load8 interruptFlag

  return mode'

-- update GPU state as long as it produces state changes
updateGPUTillDone :: MonadEmulator m => m [ Word8 ]
updateGPUTillDone = fmap reverse $ do
  x <- updateGPU
  maybe (return []) (\y -> (y :) <$> updateGPUTillDone) x

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
data LCDConfig = LCDConfig
  { windowTileTableIndex :: Word8 -> Word8 -> LoadStore8
  , windowEnabled :: Bool
  , backgroundTileIndex :: Word8 -> Word8 -> LoadStore8
  , backgroundEnabled :: Bool
  , tileAddr :: Word8 -> Word16
  , spriteSize :: Word8
  , spriteEnabled :: Bool
  }

lcdConfig :: MonadEmulator m => m (Maybe LCDConfig)
lcdConfig = do
  c <- load8 control
  return $ do
    guard (c `testBit` 7)
    return $ LCDConfig
      { windowTileTableIndex = \y x ->
          let tableIndex = fromIntegral (x `div` 8) + 32 * fromIntegral (y `div` 8)
              tableBase = if c `testBit` 6 then 0x9C00 else 0x9800
          in Addr8 $ tableBase + tableIndex
      , windowEnabled = c `testBit` 5
      , backgroundTileIndex = \y x ->
          let bgrdTableIndex = fromIntegral (x `div` 8) + 32 * fromIntegral (y `div` 8)
              bgrdTableBase = if c `testBit` 3 then 0x9C00 else 0x9800
          in Addr8 $ bgrdTableBase + bgrdTableIndex
      , backgroundEnabled = c `testBit` 0
      , tileAddr = if c `testBit` 4
        then \idx -> 0x8000 + fromIntegral idx `shiftL` 4
        else \idx -> 0x8800 + fromIntegral (idx + 128) `shiftL` 4
      , spriteSize = if c `testBit` 2 then 16 else 8
      , spriteEnabled = c `testBit` 1
      }

getBackgroundTileIndex :: MonadEmulator m => LCDConfig -> Word8 -> Word8 -> m Word8
getBackgroundTileIndex conf y x = load8 $ backgroundTileIndex conf y x

{-
-- sprite display?
-- colorZeroTransparent :: MonadEmulator m => m Bool
-- colorZeroTransparent = (`testBit` 1) <$> load8 control

spriteSize :: MonadEmulator m => m Word8
spriteSize = do
  spriteMode <- (`testBit` 2) <$> load8 control
  return $ if spriteMode then 16 else 8
-}

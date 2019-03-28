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
  HBlank -> update 204 (\t' -> (t' , l+1, Just $ if l == 142 then VBlank else OAM))
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
      | mode' == Just VBlank && mode == HBlank -> Just DrawImage
      | otherwise -> Nothing

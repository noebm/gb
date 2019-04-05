module GPU.Palette where

import Data.Word
import Data.Bits

newtype Palette = Palette { getPalette :: Word8 }

paletteGrayscale :: Palette -> Word8 -> Word8
paletteGrayscale p i = case paletteValue p i of
  0 -> 255
  1 -> 192
  2 -> 96
  3 -> 0
  _ -> error "impossible"

type ColorCode = Word8

paletteValue :: Palette -> ColorCode -> Word8
paletteValue (Palette p) idx = (p `shiftR` fromIntegral (2 * idx)) .&. 0x3

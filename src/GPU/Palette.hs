module GPU.Palette where

import Data.Word
import Data.Bits

newtype Palette = Palette { getPalette :: Word8 }
  deriving Show

defaultPalette :: Palette
defaultPalette = Palette 0x00

paletteGrayscale :: Palette -> Color -> Word8
paletteGrayscale p i = case paletteValue p i of
  0 -> 255
  1 -> 192
  2 -> 96
  3 -> 0
  _ -> error "impossible"

newtype Color = Color Word8
  deriving (Eq, Show)

colorOff   = Color 0
colorLight = Color 1
colorDark  = Color 2
colorOn    = Color 3

paletteValue :: Palette -> Color -> Word8
paletteValue (Palette p) (Color idx) = (p `shiftR` fromIntegral (2 * idx)) .&. 0x3

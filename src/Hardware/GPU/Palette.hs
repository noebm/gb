module Hardware.GPU.Palette where

import Data.Word
import Data.Bits

newtype Palette = Palette { getPalette :: Word8 }
  deriving Show

defaultPalette :: Palette
defaultPalette = Palette 0x00

newtype Color = Color Word8
  deriving (Eq, Show)

colorOff   = Color 0
colorLight = Color 1
colorDark  = Color 2
colorOn    = Color 3

paletteValue :: Palette -> Color -> Word8
paletteValue (Palette p) (Color idx) = (p `shiftR` fromIntegral (2 * idx)) .&. 0x3

-- object palettes are transparent for color 0
objPaletteValue :: Palette -> Color -> Maybe Word8
objPaletteValue (Palette p) (Color 0x00) = Nothing
objPaletteValue p c = Just (paletteValue p c)

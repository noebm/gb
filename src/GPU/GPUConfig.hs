module GPU.GPUConfig where

import Data.Word
import Data.Bits

-- should go from 0 to 3
data GPUMode = ModeHBlank | ModeVBlank | ModeOAM | ModeVRAM
  deriving (Enum)

newtype Palette = Palette { getPalette :: Word8 }

data GPUConfig = GPUConfig
  { gpuMode       :: GPUMode

  , gpuEnabled        :: Bool
  , gpuWindowTileMapSelect :: Bool
  , gpuWindowDisplay       :: Bool
  , gpuTileDataSelect :: Bool

  , gpuBGTileMapSelect :: Bool

  , gpuOBJSizeLarge    :: Bool
  , gpuOBJDisplay      :: Bool
  , gpuPriority        :: Bool

  , gpuYCompareInterrupt :: Bool
  , gpuOAMInterrupt      :: Bool
  , gpuVblankInterrupt   :: Bool
  , gpuHblankInterrupt   :: Bool

  , gpuYCoordinate :: Word8
  , gpuYCompare    :: Word8

  , gpuBGPalette   :: Palette
  , gpuOBJ0Palette :: Palette
  , gpuOBJ1Palette :: Palette

  , gpuScrollX :: Word8
  , gpuScrollY :: Word8
  , gpuWindowX :: Word8
  , gpuWindowY :: Word8
  }

{-# INLINE gpuModeNumber #-}
gpuModeNumber :: GPUConfig -> Word8
gpuModeNumber GPUConfig { gpuMode = m } = case m of
  ModeHBlank -> 0
  ModeVBlank -> 1
  ModeOAM    -> 2
  ModeVRAM   -> 3

gpuYAtCompare :: GPUConfig -> Bool
gpuYAtCompare GPUConfig { gpuYCoordinate = ly , gpuYCompare = lyc }
  = ly == lyc

storeGPU :: GPUConfig -> Word16 -> Word8 -> GPUConfig
storeGPU g 0xFF40 b = g
  { gpuEnabled             = b `testBit` 7
  , gpuWindowTileMapSelect = b `testBit` 6
  , gpuWindowDisplay       = b `testBit` 5
  , gpuTileDataSelect      = b `testBit` 4
  , gpuBGTileMapSelect     = b `testBit` 3
  , gpuOBJSizeLarge        = b `testBit` 2
  , gpuOBJDisplay          = b `testBit` 1
  , gpuPriority            = b `testBit` 0
  }
storeGPU g 0xFF41 b = g
  { gpuYCompareInterrupt = b `testBit` 6
  , gpuOAMInterrupt      = b `testBit` 5
  , gpuVblankInterrupt   = b `testBit` 4
  , gpuHblankInterrupt   = b `testBit` 3
  }
storeGPU g 0xFF42 b = g { gpuScrollY = b }
storeGPU g 0xFF43 b = g { gpuScrollX = b }
storeGPU g 0xFF44 _ = g
storeGPU g 0xFF45 b = g { gpuYCompare = b }
storeGPU g 0xFF46 _ = g -- ??? dma transfer ... should be handled separately
storeGPU g 0xFF47 b = g { gpuBGPalette   = Palette b } -- non CBG mode only
storeGPU g 0xFF48 b = g { gpuOBJ0Palette = Palette b } -- non CBG mode only
storeGPU g 0xFF49 b = g { gpuOBJ1Palette = Palette b } -- non CBG mode only
storeGPU g 0xFF4A b = g { gpuWindowY = b }
storeGPU g 0xFF4B b = g { gpuWindowX = b }

loadGPU :: GPUConfig -> Word16 -> Word8
loadGPU g 0xFF40 = foldl (.|.) 0x00
   [ if gpuEnabled             g then bit 7 else 0x00
   , if gpuWindowTileMapSelect g then bit 6 else 0x00
   , if gpuWindowDisplay       g then bit 5 else 0x00
   , if gpuTileDataSelect      g then bit 4 else 0x00
   , if gpuBGTileMapSelect     g then bit 3 else 0x00
   , if gpuOBJSizeLarge        g then bit 2 else 0x00
   , if gpuOBJDisplay          g then bit 1 else 0x00
   , if gpuPriority            g then bit 0 else 0x00
   ]
loadGPU g 0xFF41 = foldl (.|.) 0x00
  [ if gpuYCompareInterrupt g then bit 6 else 0x00
  , if gpuOAMInterrupt      g then bit 5 else 0x00
  , if gpuVblankInterrupt   g then bit 4 else 0x00
  , if gpuHblankInterrupt   g then bit 3 else 0x00
  , if gpuYAtCompare        g then bit 2 else 0x00
  , gpuModeNumber g
  ]
loadGPU g 0xFF42 = gpuScrollY g
loadGPU g 0xFF43 = gpuScrollX g
loadGPU g 0xFF44 = gpuYCoordinate g
loadGPU g 0xFF45  = gpuYCompare g
-- loadGPU g 0xFF46 = g -- ??? dma transfer ... should be handled separately
loadGPU g 0xFF47 = getPalette $ gpuBGPalette   g -- non CBG mode only
loadGPU g 0xFF48 = getPalette $ gpuOBJ0Palette g -- non CBG mode only
loadGPU g 0xFF49 = getPalette $ gpuOBJ1Palette g -- non CBG mode only
loadGPU g 0xFF4A = gpuWindowY g
loadGPU g 0xFF4B = gpuWindowX g

module GPU.GPUConfig
  ( GPUMode (..)
  , GPUConfig (..)
  , defaultGPUConfig
  , updateGPUConfig
  , loadGPUConfig
  , storeGPUConfig
  )
where

import Data.Word
import Data.Bits
import Control.Monad
import Text.Printf

import GPU.Palette

-- should go from 0 to 3
data GPUMode = ModeHBlank | ModeVBlank | ModeOAM | ModeVRAM
  deriving (Eq, Show)

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
  deriving (Show)

defaultGPUConfig :: GPUConfig
defaultGPUConfig = GPUConfig
  { gpuMode                = ModeHBlank
  , gpuEnabled             = False
  , gpuWindowTileMapSelect = False
  , gpuWindowDisplay       = False
  , gpuTileDataSelect      = False
  , gpuBGTileMapSelect     = False
  , gpuOBJSizeLarge        = False
  , gpuOBJDisplay          = False
  , gpuPriority            = False

  , gpuYCompareInterrupt = False
  , gpuOAMInterrupt      = False
  , gpuVblankInterrupt   = False
  , gpuHblankInterrupt   = False

  , gpuYCoordinate = 0
  , gpuYCompare    = 0

  , gpuBGPalette   = Palette 0
  , gpuOBJ0Palette = Palette 0
  , gpuOBJ1Palette = Palette 0

  , gpuScrollX = 0
  , gpuScrollY = 0
  , gpuWindowX = 0
  , gpuWindowY = 0
  }

{-# INLINE gpuModeDuration #-}
gpuModeDuration :: GPUMode -> Word
gpuModeDuration ModeHBlank = 204
gpuModeDuration ModeVBlank = 456
gpuModeDuration ModeOAM    = 80
gpuModeDuration ModeVRAM   = 172

clearInterrupts :: GPUConfig -> GPUConfig
clearInterrupts gpu = gpu
  { gpuOAMInterrupt      = False
  , gpuVblankInterrupt   = False
  , gpuHblankInterrupt   = False
  , gpuYCompareInterrupt = False
  }

updateStatusInterrupts :: GPUConfig -> GPUConfig
updateStatusInterrupts gpu = case gpuMode gpu of
  ModeOAM    -> gpu' { gpuOAMInterrupt = True, gpuYCompareInterrupt = gpuYAtCompare gpu }
  ModeHBlank -> gpu' { gpuHblankInterrupt = True }
  ModeVBlank -> gpu' { gpuVblankInterrupt = True, gpuYCompareInterrupt = gpuYAtCompare gpu }
  _ -> gpu'
  where gpu' = clearInterrupts gpu

updateGPUConfig :: Word -> GPUConfig -> Maybe (Word, GPUConfig)
updateGPUConfig cycles g = do
  let cyclesMode = gpuModeDuration (gpuMode g)
  guard (cycles >= cyclesMode)
  return (cycles - cyclesMode, updateStatusInterrupts $ gpuNextConfig g)

gpuNextConfig :: GPUConfig -> GPUConfig
gpuNextConfig g = case gpuMode g of
  ModeHBlank -> if y == 143
                then g { gpuMode = ModeVBlank , gpuYCoordinate = y + 1 }
                else g { gpuMode = ModeOAM    , gpuYCoordinate = y + 1 }
  ModeVBlank -> if gpuYCoordinate g == 153
                then g { gpuMode = ModeOAM , gpuYCoordinate = 0 }
                else g { gpuYCoordinate = y + 1 }
  ModeOAM    -> g { gpuMode = ModeVRAM   }
  ModeVRAM   -> g { gpuMode = ModeHBlank }
  where y = gpuYCoordinate g

{-# INLINE gpuModeNumber #-}
gpuModeNumber :: GPUConfig -> Word8
gpuModeNumber GPUConfig { gpuMode = m } = case m of
  ModeHBlank -> 0
  ModeVBlank -> 1
  ModeOAM    -> 2
  ModeVRAM   -> 3

{-# INLINE gpuYAtCompare #-}
gpuYAtCompare :: GPUConfig -> Bool
gpuYAtCompare GPUConfig { gpuYCoordinate = ly , gpuYCompare = lyc }
  = ly == lyc

storeGPUConfig :: GPUConfig -> Word16 -> Word8 -> GPUConfig
storeGPUConfig g 0xFF40 b = g
  { gpuEnabled             = b `testBit` 7
  , gpuWindowTileMapSelect = b `testBit` 6
  , gpuWindowDisplay       = b `testBit` 5
  , gpuTileDataSelect      = b `testBit` 4
  , gpuBGTileMapSelect     = b `testBit` 3
  , gpuOBJSizeLarge        = b `testBit` 2
  , gpuOBJDisplay          = b `testBit` 1
  , gpuPriority            = b `testBit` 0
  }
storeGPUConfig g 0xFF41 b = g
  { gpuYCompareInterrupt = b `testBit` 6
  , gpuOAMInterrupt      = b `testBit` 5
  , gpuVblankInterrupt   = b `testBit` 4
  , gpuHblankInterrupt   = b `testBit` 3
  }
storeGPUConfig g 0xFF42 b = g { gpuScrollY = b }
storeGPUConfig g 0xFF43 b = g { gpuScrollX = b }
storeGPUConfig g 0xFF44 _ = g
storeGPUConfig g 0xFF45 b = g { gpuYCompare = b }
-- storeGPUConfig g 0xFF46 _ = g -- ??? dma transfer ... should be handled separately
storeGPUConfig g 0xFF47 b = g { gpuBGPalette   = Palette b } -- non CBG mode only
storeGPUConfig g 0xFF48 b = g { gpuOBJ0Palette = Palette b } -- non CBG mode only
storeGPUConfig g 0xFF49 b = g { gpuOBJ1Palette = Palette b } -- non CBG mode only
storeGPUConfig g 0xFF4A b = g { gpuWindowY = b }
storeGPUConfig g 0xFF4B b = g { gpuWindowX = b }
storeGPUConfig _ _ _ = error "storeGPUConfig: not in range"

loadGPUConfig :: GPUConfig -> Word16 -> Word8
loadGPUConfig g 0xFF40 = foldl (.|.) 0x00
   [ if gpuEnabled             g then bit 7 else 0x00
   , if gpuWindowTileMapSelect g then bit 6 else 0x00
   , if gpuWindowDisplay       g then bit 5 else 0x00
   , if gpuTileDataSelect      g then bit 4 else 0x00
   , if gpuBGTileMapSelect     g then bit 3 else 0x00
   , if gpuOBJSizeLarge        g then bit 2 else 0x00
   , if gpuOBJDisplay          g then bit 1 else 0x00
   , if gpuPriority            g then bit 0 else 0x00
   ]
loadGPUConfig g 0xFF41 = foldl (.|.) 0x80
  [ if gpuYCompareInterrupt g then bit 6 else 0x00
  , if gpuOAMInterrupt      g then bit 5 else 0x00
  , if gpuVblankInterrupt   g then bit 4 else 0x00
  , if gpuHblankInterrupt   g then bit 3 else 0x00
  , if gpuYAtCompare        g then bit 2 else 0x00
  , if gpuEnabled g then gpuModeNumber g else 0x00
  ]
loadGPUConfig g 0xFF42 = gpuScrollY g
loadGPUConfig g 0xFF43 = gpuScrollX g
loadGPUConfig g 0xFF44 = gpuYCoordinate g
loadGPUConfig g 0xFF45  = gpuYCompare g
-- loadGPUConfig g 0xFF46 = g -- ??? dma transfer ... should be handled separately
loadGPUConfig g 0xFF47 = getPalette $ gpuBGPalette   g -- non CBG mode only
loadGPUConfig g 0xFF48 = getPalette $ gpuOBJ0Palette g -- non CBG mode only
loadGPUConfig g 0xFF49 = getPalette $ gpuOBJ1Palette g -- non CBG mode only
loadGPUConfig g 0xFF4A = gpuWindowY g
loadGPUConfig g 0xFF4B = gpuWindowX g
loadGPUConfig _ 0xff4d = 0xff
loadGPUConfig _ addr = error $ printf "loadGPUConfig: not in range 0x%04x" addr

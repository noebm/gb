{-# LANGUAGE TemplateHaskell #-}
module GPU.GPUControl
  ( GPUMode (..)
  , GPUControl (..)
  , defaultGPUControl
  , updateGPUControl
  , loadGPUControl
  , storeGPUControl

  , gpuEnabled
  , gpuTileDataSelect
  , gpuWindowTileMapSelect, gpuWindowDisplay
  , gpuBGTileMapSelect, gpuBGDisplay
  , gpuOBJSizeLarge, gpuOBJDisplay

  , gpuYCoordinate
  , gpuScroll

  )
where

import Data.Word
import Data.Bits
import Control.Monad
import Text.Printf

import GPU.Palette
import SDL.Vect

import Control.Lens
import Data.Bits.Lens

-- should go from 0 to 3
data GPUMode = ModeHBlank | ModeVBlank | ModeOAM | ModeVRAM
  deriving (Eq, Show)

data GPUControl = GPUControl
  { _gpuMode       :: GPUMode
  , _gpuClock      :: !Word

  , _gpuLCDControlByte :: Word8

  , _gpuYCompareInterrupt :: Bool
  , _gpuOAMInterrupt      :: Bool
  , _gpuVblankInterrupt   :: Bool
  , _gpuHblankInterrupt   :: Bool

  , _gpuYCoordinate :: Word8
  , _gpuYCompare    :: Word8

  , _gpuBGPalette   :: Palette
  , _gpuOBJ0Palette :: Palette
  , _gpuOBJ1Palette :: Palette

  , _gpuScroll :: V2 Word8
  , _gpuWindow :: V2 Word8
  }
  deriving (Show)

makePrisms ''GPUMode
makeLenses ''GPUControl

gpuEnabled, gpuWindowTileMapSelect, gpuWindowDisplay, gpuTileDataSelect,
  gpuBGTileMapSelect, gpuOBJSizeLarge, gpuOBJDisplay, gpuBGDisplay :: Lens' GPUControl Bool
gpuEnabled             = gpuLCDControlByte . bitAt 7
gpuWindowTileMapSelect = gpuLCDControlByte . bitAt 6
gpuWindowDisplay       = gpuLCDControlByte . bitAt 5
gpuTileDataSelect      = gpuLCDControlByte . bitAt 4
gpuBGTileMapSelect     = gpuLCDControlByte . bitAt 3
gpuOBJSizeLarge        = gpuLCDControlByte . bitAt 2
gpuOBJDisplay          = gpuLCDControlByte . bitAt 1
gpuBGDisplay           = gpuLCDControlByte . bitAt 0

defaultGPUControl :: GPUControl
defaultGPUControl = GPUControl
  { _gpuMode                = ModeHBlank
  , _gpuClock               = 0
  , _gpuLCDControlByte      = 0x00

  , _gpuYCompareInterrupt = False
  , _gpuOAMInterrupt      = False
  , _gpuVblankInterrupt   = False
  , _gpuHblankInterrupt   = False

  , _gpuYCoordinate = 0
  , _gpuYCompare    = 0

  , _gpuBGPalette   = Palette 0
  , _gpuOBJ0Palette = Palette 0
  , _gpuOBJ1Palette = Palette 0

  , _gpuScroll = zero
  , _gpuWindow = zero
  }

{-# INLINE gpuModeDuration #-}
gpuModeDuration :: GPUMode -> Word
gpuModeDuration ModeHBlank = 204
gpuModeDuration ModeVBlank = 456
gpuModeDuration ModeOAM    = 80
gpuModeDuration ModeVRAM   = 172

clearInterrupts :: GPUControl -> GPUControl
clearInterrupts gpu = gpu
  { _gpuOAMInterrupt      = False
  , _gpuVblankInterrupt   = False
  , _gpuHblankInterrupt   = False
  , _gpuYCompareInterrupt = False
  }

updateStatusInterrupts :: GPUControl -> (Bool, GPUControl)
updateStatusInterrupts gpu = case _gpuMode gpu of
  ModeOAM    -> (True, gpu' { _gpuOAMInterrupt = True, _gpuYCompareInterrupt = gpuYAtCompare gpu })
  ModeHBlank -> (True, gpu' { _gpuHblankInterrupt = True })
  ModeVBlank -> let f1 = _gpuYCoordinate gpu == 144
                    f2 = gpuYAtCompare gpu
                in ( f1 || f2
                   , gpu' { _gpuVblankInterrupt = f1, _gpuYCompareInterrupt = f2 } )
  _ -> (False, gpu')
  where gpu' = clearInterrupts gpu

updateGPUControl :: Word -> GPUControl -> (Bool, GPUControl)
updateGPUControl cycles g =
  let cyclesMode = gpuModeDuration (_gpuMode g)
      g' = g { _gpuClock = _gpuClock g + cycles }
  in if _gpuClock g' >= cyclesMode
     then updateStatusInterrupts $ gpuNextConfig $ g' { _gpuClock = _gpuClock g' - cyclesMode }
     else (False , g')

gpuNextConfig :: GPUControl -> GPUControl
gpuNextConfig g = case _gpuMode g of
  ModeHBlank -> if y < 143
                then g { _gpuMode = ModeOAM    , _gpuYCoordinate = y + 1 }
                else g { _gpuMode = ModeVBlank , _gpuYCoordinate = y + 1 }
  ModeVBlank -> if _gpuYCoordinate g < 153
                then g { _gpuYCoordinate = y + 1 }
                else g { _gpuMode = ModeOAM , _gpuYCoordinate = 0 }
  ModeOAM    -> g { _gpuMode = ModeVRAM   }
  ModeVRAM   -> g { _gpuMode = ModeHBlank }
  where y = _gpuYCoordinate g

{-# INLINE gpuModeNumber #-}
gpuModeNumber :: GPUControl -> Word8
gpuModeNumber GPUControl { _gpuMode = m } = case m of
  ModeHBlank -> 0
  ModeVBlank -> 1
  ModeOAM    -> 2
  ModeVRAM   -> 3

{-# INLINE gpuYAtCompare #-}
gpuYAtCompare :: GPUControl -> Bool
gpuYAtCompare GPUControl { _gpuYCoordinate = ly , _gpuYCompare = lyc }
  = ly == lyc

storeGPUControl :: GPUControl -> Word16 -> Word8 -> GPUControl
storeGPUControl g 0xFF40 b = g & gpuLCDControlByte .~ b
storeGPUControl g 0xFF41 b = g
  { _gpuYCompareInterrupt = b `testBit` 6
  , _gpuOAMInterrupt      = b `testBit` 5
  , _gpuVblankInterrupt   = b `testBit` 4
  , _gpuHblankInterrupt   = b `testBit` 3
  }
storeGPUControl g 0xFF42 b = g & gpuScroll._y .~ b
storeGPUControl g 0xFF43 b = g & gpuScroll._x .~ b
storeGPUControl g 0xFF44 _ = g
storeGPUControl g 0xFF45 b = g { _gpuYCompare = b }
-- storeGPUControl g 0xFF46 _ = g -- ??? dma transfer ... should be handled separately
storeGPUControl g 0xFF47 b = g { _gpuBGPalette   = Palette b } -- non CBG mode only
storeGPUControl g 0xFF48 b = g { _gpuOBJ0Palette = Palette b } -- non CBG mode only
storeGPUControl g 0xFF49 b = g { _gpuOBJ1Palette = Palette b } -- non CBG mode only
storeGPUControl g 0xFF4A b = g & gpuWindow._y .~ b
storeGPUControl g 0xFF4B b = g & gpuWindow._x .~ b
storeGPUControl _ _ _ = error "storeGPUControl: not in range"

loadGPUControl :: GPUControl -> Word16 -> Word8
loadGPUControl g 0xFF40 = g ^. gpuLCDControlByte
loadGPUControl g 0xFF41 = foldl (.|.) 0x80
  [ if _gpuYCompareInterrupt g then bit 6 else 0x00
  , if _gpuOAMInterrupt      g then bit 5 else 0x00
  , if _gpuVblankInterrupt   g then bit 4 else 0x00
  , if _gpuHblankInterrupt   g then bit 3 else 0x00
  , if gpuYAtCompare        g then bit 2 else 0x00
  , if view gpuEnabled g then gpuModeNumber g else 0x00
  ]
loadGPUControl g 0xFF42 = g ^. gpuScroll._y
loadGPUControl g 0xFF43 = g ^. gpuScroll._x
loadGPUControl g 0xFF44 = _gpuYCoordinate g
loadGPUControl g 0xFF45  = _gpuYCompare g
-- loadGPUControl g 0xFF46 = g -- ??? dma transfer ... should be handled separately
loadGPUControl g 0xFF47 = getPalette $ _gpuBGPalette   g -- non CBG mode only
loadGPUControl g 0xFF48 = getPalette $ _gpuOBJ0Palette g -- non CBG mode only
loadGPUControl g 0xFF49 = getPalette $ _gpuOBJ1Palette g -- non CBG mode only
loadGPUControl g 0xFF4A = g ^. gpuWindow._y
loadGPUControl g 0xFF4B = g ^. gpuWindow._x
loadGPUControl _ 0xff4d = 0xff
loadGPUControl _ addr = error $ printf "loadGPUControl: not in range 0x%04x" addr

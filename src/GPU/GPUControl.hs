{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module GPU.GPUControl
  ( GPUMode (..)
  , GPUControl (..)
  , GPURequest (..)
  , defaultGPUControl
  , updateGPUControl
  , loadGPUControl
  , storeGPUControl

  , gpuEnabled
  , gpuTileDataSelect
  , gpuWindowTileMapSelect, displayWindow
  , gpuBGTileMapSelect, displayBG
  , gpuOBJSizeLarge, displayOBJ

  , gpuOBJPalette

  , gpuMode
  , gpuLine
  , gpuScroll
  , gpuWindow

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

{-# INLINE gpuModeNumber #-}
gpuModeNumber :: GPUControl -> Word8
gpuModeNumber GPUControl { _gpuMode = m } = case m of
  ModeHBlank -> 0
  ModeVBlank -> 1
  ModeOAM    -> 2
  ModeVRAM   -> 3

{-# INLINE gpuModeDuration #-}
gpuModeDuration :: GPUMode -> Word
gpuModeDuration ModeHBlank = 204
gpuModeDuration ModeVBlank = 456
gpuModeDuration ModeOAM    = 80
gpuModeDuration ModeVRAM   = 172

data GPUControl = GPUControl
  { _gpuClock      :: !Word

  , _gpuLCDControlByte :: Word8

  , _gpuLineCompareInterrupt :: Bool
  , _gpuOAMInterrupt         :: Bool
  , _gpuVblankInterrupt      :: Bool
  , _gpuHblankInterrupt      :: Bool
  , _gpuMode                 :: GPUMode

  , _gpuLineCompare    :: Word8
  , _gpuLine :: Word8

  , _gpuBGPalette   :: Palette
  , _gpuOBJ0Palette :: Palette
  , _gpuOBJ1Palette :: Palette

  , _gpuScroll :: V2 Word8
  , _gpuWindow :: V2 Word8
  }
  deriving (Show)

makePrisms ''GPUMode
makeLenses ''GPUControl

gpuEnabled, gpuWindowTileMapSelect, displayWindow, gpuTileDataSelect,
  gpuBGTileMapSelect, gpuOBJSizeLarge, displayOBJ, displayBG :: Lens' GPUControl Bool
gpuEnabled             = gpuLCDControlByte . bitAt 7
gpuWindowTileMapSelect = gpuLCDControlByte . bitAt 6
displayWindow          = gpuLCDControlByte . bitAt 5
gpuTileDataSelect      = gpuLCDControlByte . bitAt 4
gpuBGTileMapSelect     = gpuLCDControlByte . bitAt 3
gpuOBJSizeLarge        = gpuLCDControlByte . bitAt 2
displayOBJ             = gpuLCDControlByte . bitAt 1
displayBG              = gpuLCDControlByte . bitAt 0

gpuOBJPalette :: Bool -> Lens' GPUControl Palette
gpuOBJPalette f = if f then gpuOBJ1Palette else gpuOBJ0Palette

defaultGPUControl :: GPUControl
defaultGPUControl = GPUControl
  { _gpuClock               = 0
  , _gpuLCDControlByte      = 0x00

  , _gpuLineCompareInterrupt = False
  , _gpuOAMInterrupt      = False
  , _gpuVblankInterrupt   = False
  , _gpuHblankInterrupt   = False
  , _gpuMode              = ModeHBlank

  , _gpuLineCompare       = 0
  , _gpuLine              = 0

  , _gpuBGPalette   = Palette 0
  , _gpuOBJ0Palette = Palette 0
  , _gpuOBJ1Palette = Palette 0

  , _gpuScroll = zero
  , _gpuWindow = zero
  }

data GPURequest = Draw | NewLine
  deriving (Eq)

lineInterrupt gpu = (gpu ^. gpuLineCompareInterrupt && gpuYAtCompare gpu)

-- returns stat interrupt, renderer requests and new state
updateGPUControl :: Word -> GPUControl -> (Bool, Maybe GPURequest, GPUControl)
updateGPUControl cycles g =
  let cyclesMode = gpuModeDuration (_gpuMode g)
      g' = g { _gpuClock = _gpuClock g + cycles }
  in if _gpuClock g' >= cyclesMode
     then gpuNextConfig $ g' { _gpuClock = _gpuClock g' - cyclesMode }
     else (False, Nothing, g')

gpuNextConfig :: GPUControl -> (Bool, Maybe GPURequest, GPUControl)
gpuNextConfig g = case _gpuMode g of
  ModeHBlank ->
    let (f, req, g') = if y < 143
                       then (g ^. gpuOAMInterrupt   , Nothing  , g & gpuMode .~ ModeOAM    & gpuLine +~ 1)
                       else (g ^. gpuVblankInterrupt, Just Draw, g & gpuMode .~ ModeVBlank & gpuLine +~ 1)
    in (f || lineInterrupt g', req, g')
  ModeVBlank ->
    let (f, req, g') = if _gpuLine g < 153
                       then (False, Nothing, g & gpuLine +~ 1)
                       else (g ^. gpuOAMInterrupt, Nothing, g & gpuMode .~ ModeOAM & gpuLine .~ 0)
    in (f || lineInterrupt g', req, g')
  ModeOAM    -> (False , Nothing, g & gpuMode .~ ModeVRAM)
  ModeVRAM   -> (g ^. gpuHblankInterrupt, Just NewLine, g & gpuMode .~ ModeHBlank)
  where y = _gpuLine g

{-# INLINE gpuYAtCompare #-}
gpuYAtCompare :: GPUControl -> Bool
gpuYAtCompare GPUControl { _gpuLine = ly , _gpuLineCompare = lyc }
  = ly == lyc

storeGPUControl :: GPUControl -> Word16 -> Word8 -> GPUControl
storeGPUControl g 0xFF40 b = g & gpuLCDControlByte .~ b
storeGPUControl g 0xFF41 b = g
  { _gpuLineCompareInterrupt = b `testBit` 6
  , _gpuOAMInterrupt      = b `testBit` 5
  , _gpuVblankInterrupt   = b `testBit` 4
  , _gpuHblankInterrupt   = b `testBit` 3
  }
storeGPUControl g 0xFF42 b = g & gpuScroll._y .~ b
storeGPUControl g 0xFF43 b = g & gpuScroll._x .~ b
storeGPUControl g 0xFF44 _ = g
storeGPUControl g 0xFF45 b = g & gpuLineCompare .~ b
-- storeGPUControl g 0xFF46 _ = g -- ??? dma transfer ... should be handled separately
storeGPUControl g 0xFF47 b = g & gpuBGPalette   .~ Palette b -- non CBG mode only
storeGPUControl g 0xFF48 b = g & gpuOBJ0Palette .~ Palette b -- non CBG mode only
storeGPUControl g 0xFF49 b = g & gpuOBJ1Palette .~ Palette b -- non CBG mode only
storeGPUControl g 0xFF4A b = g & gpuWindow._y .~ b
storeGPUControl g 0xFF4B b = g & gpuWindow._x .~ b
storeGPUControl _ _ _ = error "storeGPUControl: not in range"

loadGPUControl :: GPUControl -> Word16 -> Word8
loadGPUControl g 0xFF40 = g ^. gpuLCDControlByte
loadGPUControl g 0xFF41 = 0x80
  .|. (if g ^. gpuEnabled then gpuModeNumber g else 0x00)
  & bitAt 6 .~ (g ^. gpuLineCompareInterrupt)
  & bitAt 5 .~ (g ^. gpuOAMInterrupt)
  & bitAt 4 .~ (g ^. gpuVblankInterrupt)
  & bitAt 3 .~ (g ^. gpuHblankInterrupt)
  & bitAt 2 .~ gpuYAtCompare g
loadGPUControl g 0xFF42 = g ^. gpuScroll._y
loadGPUControl g 0xFF43 = g ^. gpuScroll._x
loadGPUControl g 0xFF44 = _gpuLine g
loadGPUControl g 0xFF45  = _gpuLineCompare g
-- loadGPUControl g 0xFF46 = g -- ??? dma transfer ... should be handled separately
loadGPUControl g 0xFF47 = getPalette $ _gpuBGPalette   g -- non CBG mode only
loadGPUControl g 0xFF48 = getPalette $ _gpuOBJ0Palette g -- non CBG mode only
loadGPUControl g 0xFF49 = getPalette $ _gpuOBJ1Palette g -- non CBG mode only
loadGPUControl g 0xFF4A = g ^. gpuWindow._y
loadGPUControl g 0xFF4B = g ^. gpuWindow._x
loadGPUControl _ 0xff4d = 0xff
loadGPUControl _ addr = error $ printf "loadGPUControl: not in range 0x%04x" addr

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.GPU.GPUControl
  ( GPUMode (..)
  , GPUControl (..)
  , GPURequest (..)
  , defaultGPUControl
  , updateGPUControl
  , loadGPUControl
  , storeGPUControl

  , canAccessGPURAM, canAccessOAM

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
import Text.Printf

import Hardware.GPU.Palette
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
  , _gpuDMAAddress :: Word8
  }
  deriving (Show)

makePrisms ''GPUMode
makeLenses ''GPUControl

{-# INLINE gpuEnabled             #-}
{-# INLINE gpuWindowTileMapSelect #-}
{-# INLINE displayWindow          #-}
{-# INLINE gpuTileDataSelect      #-}
{-# INLINE gpuBGTileMapSelect     #-}
{-# INLINE gpuOBJSizeLarge        #-}
{-# INLINE displayOBJ             #-}
{-# INLINE displayBG              #-}
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

{-# INLINE gpuOBJPalette #-}
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
  , _gpuMode              = ModeOAM

  , _gpuLineCompare       = 0
  , _gpuLine              = 0

  , _gpuBGPalette   = Palette 0
  , _gpuOBJ0Palette = Palette 0
  , _gpuOBJ1Palette = Palette 0
  , _gpuDMAAddress = 0x00

  , _gpuScroll = zero
  , _gpuWindow = zero
  }

data GPURequest = Draw | NewLine
  deriving (Eq)

lineInterrupt :: GPUControl -> Bool
lineInterrupt gpu = gpu ^. gpuLineCompareInterrupt && gpuYAtCompare gpu

{-# INLINE canAccessGPURAM #-}
canAccessGPURAM :: GPUControl -> Bool
canAccessGPURAM g = isn't _ModeVRAM (g ^. gpuMode)

{-# INLINE canAccessOAM #-}
canAccessOAM :: GPUControl -> Bool
canAccessOAM g = canAccessGPURAM g && isn't _ModeOAM (g ^. gpuMode)

-- returns stat interrupt, renderer requests and new state
updateGPUControl :: Word -> GPUControl -> (Bool, Maybe GPURequest, GPUControl)
updateGPUControl cycles g
  | g ^. gpuEnabled =
  let cyclesMode = gpuModeDuration (_gpuMode g)
      g' = g { _gpuClock = _gpuClock g + cycles }
  in if _gpuClock g' >= cyclesMode
     then gpuNextConfig $ g' { _gpuClock = _gpuClock g' - cyclesMode }
     else (False, Nothing, g')
  | otherwise = (False, Nothing, g)

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

storeGPUControl :: Word16 -> Word8 -> GPUControl -> GPUControl
storeGPUControl 0xFF40 b g = g & gpuLCDControlByte .~ b
storeGPUControl 0xFF41 b g = g
  { _gpuLineCompareInterrupt = b `testBit` 6
  , _gpuOAMInterrupt      = b `testBit` 5
  , _gpuVblankInterrupt   = b `testBit` 4
  , _gpuHblankInterrupt   = b `testBit` 3
  }
storeGPUControl 0xFF42 b g = g & gpuScroll._y .~ b
storeGPUControl 0xFF43 b g = g & gpuScroll._x .~ b
storeGPUControl 0xFF44 _ g = g
storeGPUControl 0xFF45 b g = g & gpuLineCompare .~ b
storeGPUControl 0xFF46 b g = g & gpuDMAAddress .~ b
storeGPUControl 0xFF47 b g = g & gpuBGPalette   .~ Palette b -- non CBG mode only
storeGPUControl 0xFF48 b g = g & gpuOBJ0Palette .~ Palette b -- non CBG mode only
storeGPUControl 0xFF49 b g = g & gpuOBJ1Palette .~ Palette b -- non CBG mode only
storeGPUControl 0xFF4A b g = g & gpuWindow._y .~ b
storeGPUControl 0xFF4B b g = g & gpuWindow._x .~ b
storeGPUControl _ _ _ = error "storeGPUControl: not in range"

loadGPUControl :: Word16 -> GPUControl -> Word8
loadGPUControl 0xFF40 g = g ^. gpuLCDControlByte
loadGPUControl 0xFF41 g = 0x80
  .|. (if g ^. gpuEnabled then gpuModeNumber g else 0x00)
  & bitAt 6 .~ (g ^. gpuLineCompareInterrupt)
  & bitAt 5 .~ (g ^. gpuOAMInterrupt)
  & bitAt 4 .~ (g ^. gpuVblankInterrupt)
  & bitAt 3 .~ (g ^. gpuHblankInterrupt)
  & bitAt 2 .~ gpuYAtCompare g
loadGPUControl 0xFF42 g = g ^. gpuScroll._y
loadGPUControl 0xFF43 g = g ^. gpuScroll._x
loadGPUControl 0xFF44 g = _gpuLine g
loadGPUControl 0xFF45 g = _gpuLineCompare g
loadGPUControl 0xFF46 g = _gpuDMAAddress g
loadGPUControl 0xFF47 g = getPalette $ _gpuBGPalette   g -- non CBG mode only
loadGPUControl 0xFF48 g = getPalette $ _gpuOBJ0Palette g -- non CBG mode only
loadGPUControl 0xFF49 g = getPalette $ _gpuOBJ1Palette g -- non CBG mode only
loadGPUControl 0xFF4A g = g ^. gpuWindow._y
loadGPUControl 0xFF4B g = g ^. gpuWindow._x
loadGPUControl 0xff4d _ = 0xff
loadGPUControl addr _ = error $ printf "loadGPUControl: not in range 0x%04x" addr

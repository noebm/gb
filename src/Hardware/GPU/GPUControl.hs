{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.GPU.GPUControl
  ( GPUMode(..)
  , GPUControl(..)
  , GPURequest(..)
  , defaultGPUControl
  , updateGPUControl
  , loadGPUControl
  , storeGPUControl
  , canAccessGPURAM
  , canAccessOAM
  , gpuEnabled
  , gpuTileDataSelect
  , gpuWindowTileMapSelect
  , displayWindow
  , gpuBGTileMapSelect
  , displayBG
  , gpuOBJSizeLarge
  , displayOBJ
  , gpuOBJPalette
  , gpuMode
  , gpuLine
  , gpuScroll
  , gpuWindow
  ) where

import           Data.Bits
import           Data.Word
import           Text.Printf

import           Hardware.GPU.Palette
import           SDL.Vect

import           Control.Lens
import           Data.Bits.Lens

import           Control.Monad.State.Strict

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
  { _gpuClock                :: !Word
  , _gpuLCDControlByte       :: Word8
  , _gpuLineCompareInterrupt :: Bool
  , _gpuOAMInterrupt         :: Bool
  , _gpuVblankInterrupt      :: Bool
  , _gpuHblankInterrupt      :: Bool
  , _gpuMode                 :: GPUMode
  , _gpuLineCompare          :: Word8
  , _gpuLine                 :: Word8
  , _gpuBGPalette            :: Palette
  , _gpuOBJ0Palette          :: Palette
  , _gpuOBJ1Palette          :: Palette
  , _gpuScroll               :: V2 Word8
  , _gpuWindow               :: V2 Word8
  , _gpuDMAAddress           :: Word8
  }
  deriving Show

makePrisms ''GPUMode
makeLenses ''GPUControl

{-# INLINE gpuEnabled #-}
{-# INLINE gpuWindowTileMapSelect #-}
{-# INLINE displayWindow #-}
{-# INLINE gpuTileDataSelect #-}
{-# INLINE gpuBGTileMapSelect #-}
{-# INLINE gpuOBJSizeLarge #-}
{-# INLINE displayOBJ #-}
{-# INLINE displayBG #-}
gpuEnabled, gpuWindowTileMapSelect, displayWindow, gpuTileDataSelect, gpuBGTileMapSelect, gpuOBJSizeLarge, displayOBJ, displayBG
  :: Lens' GPUControl Bool
gpuEnabled = gpuLCDControlByte . bitAt 7
gpuWindowTileMapSelect = gpuLCDControlByte . bitAt 6
displayWindow = gpuLCDControlByte . bitAt 5
gpuTileDataSelect = gpuLCDControlByte . bitAt 4
gpuBGTileMapSelect = gpuLCDControlByte . bitAt 3
gpuOBJSizeLarge = gpuLCDControlByte . bitAt 2
displayOBJ = gpuLCDControlByte . bitAt 1
displayBG = gpuLCDControlByte . bitAt 0

{-# INLINE gpuOBJPalette #-}
gpuOBJPalette :: Bool -> Lens' GPUControl Palette
gpuOBJPalette f = if f then gpuOBJ1Palette else gpuOBJ0Palette

defaultGPUControl :: GPUControl
defaultGPUControl = GPUControl { _gpuClock                = 0
                               , _gpuLCDControlByte       = 0x00
                               , _gpuLineCompareInterrupt = False
                               , _gpuOAMInterrupt         = False
                               , _gpuVblankInterrupt      = False
                               , _gpuHblankInterrupt      = False
                               , _gpuMode                 = ModeOAM
                               , _gpuLineCompare          = 0
                               , _gpuLine                 = 0
                               , _gpuBGPalette            = Palette 0
                               , _gpuOBJ0Palette          = Palette 0
                               , _gpuOBJ1Palette          = Palette 0
                               , _gpuDMAAddress           = 0x00
                               , _gpuScroll               = zero
                               , _gpuWindow               = zero
                               }

data GPURequest = Draw | NewLine
  deriving (Eq, Show)

lineInterrupt :: GPUControl -> Bool
lineInterrupt gpu = gpu ^. gpuLineCompareInterrupt && gpuYAtCompare gpu

{-# INLINE canAccessGPURAM #-}
canAccessGPURAM :: GPUControl -> Bool
canAccessGPURAM g = isn't _ModeVRAM (g ^. gpuMode)

{-# INLINE canAccessOAM #-}
canAccessOAM :: GPUControl -> Bool
canAccessOAM g = canAccessGPURAM g && isn't _ModeOAM (g ^. gpuMode)

-- returns stat interrupt, renderer requests and new state
updateGPUControl
  :: Word -> GPUControl -> ((Bool, Maybe GPURequest), GPUControl)
updateGPUControl cycles = runState $ do
  enabled <- use gpuEnabled
  if enabled
    then do
      clock      <- gpuClock <+= cycles
      cyclesMode <- uses gpuMode gpuModeDuration
      if clock >= cyclesMode
        then do
          gpuClock -= cyclesMode
          gpuNextConfig
        else return (False, Nothing)
    else return (False, Nothing)

setLine :: Word8 -> State GPUControl Bool
setLine y = do
  gpuLine .= y
  use (to lineInterrupt)

setMode :: GPUMode -> State GPUControl (Bool, Maybe GPURequest)
setMode mode = do
  gpuMode .= mode
  int <- case mode of
    ModeOAM    -> use gpuOAMInterrupt
    ModeHBlank -> use gpuHblankInterrupt
    ModeVRAM   -> return False
    ModeVBlank -> use gpuVblankInterrupt
  let req = case mode of
        ModeVBlank -> Just Draw
        ModeHBlank -> Just NewLine
        _          -> Nothing
  return (int, req)

gpuNextConfig :: State GPUControl (Bool, Maybe GPURequest)
gpuNextConfig = do
  mode <- use gpuMode
  case mode of
    ModeHBlank -> do
      y          <- use gpuLine
      flgLine    <- setLine (y + 1)
      (flg, req) <- setMode $! if y < 143 then ModeOAM else ModeVBlank
      return (flg || flgLine, req)
    ModeVBlank -> do
      y          <- use gpuLine
      flgLine    <- setLine $! if y < 153 then y + 1 else 0
      (flg, req) <- if y < 153 then return (False, Nothing) else setMode ModeOAM
      return (flg || flgLine, req)
    ModeOAM  -> setMode ModeVRAM
    ModeVRAM -> setMode ModeHBlank

{-# INLINE gpuYAtCompare #-}
gpuYAtCompare :: GPUControl -> Bool
gpuYAtCompare GPUControl { _gpuLine = ly, _gpuLineCompare = lyc } = ly == lyc

storeGPUControl :: Word16 -> Word8 -> GPUControl -> GPUControl
storeGPUControl 0xFF40 b g = g &~ do
  enabled <- use gpuEnabled
  gpuLCDControlByte .= b
  enabled' <- use gpuEnabled
  -- gpu enable
  when (not enabled && enabled') $ do
    gpuClock .= 0
    gpuMode .= ModeOAM
    -- gpuLineAtCompare .= True

  -- gpu disable
  when (enabled && not enabled') $ do
    gpuLine .= 0
    mode <- use gpuMode
    unless (mode == ModeVBlank) $ error "gpu shutdown not in VBLANK"

storeGPUControl 0xFF41 b g = g { _gpuLineCompareInterrupt = b `testBit` 6
                               , _gpuOAMInterrupt         = b `testBit` 5
                               , _gpuVblankInterrupt      = b `testBit` 4
                               , _gpuHblankInterrupt      = b `testBit` 3
                               }
storeGPUControl 0xFF42 b g = g & gpuScroll . _y .~ b
storeGPUControl 0xFF43 b g = g & gpuScroll . _x .~ b
storeGPUControl 0xFF44 _ g = g
storeGPUControl 0xFF45 b g = g & gpuLineCompare .~ b
storeGPUControl 0xFF46 b g = g & gpuDMAAddress .~ b
storeGPUControl 0xFF47 b g = g & gpuBGPalette .~ Palette b -- non CBG mode only
storeGPUControl 0xFF48 b g = g & gpuOBJ0Palette .~ Palette b -- non CBG mode only
storeGPUControl 0xFF49 b g = g & gpuOBJ1Palette .~ Palette b -- non CBG mode only
storeGPUControl 0xFF4A b g = g & gpuWindow . _y .~ b
storeGPUControl 0xFF4B b g = g & gpuWindow . _x .~ b
storeGPUControl _      _ _ = error "storeGPUControl: not in range"

loadGPUControl :: Word16 -> GPUControl -> Word8
loadGPUControl 0xFF40 g = g ^. gpuLCDControlByte
loadGPUControl 0xFF41 g =
  0x80
    .|. (if g ^. gpuEnabled then gpuModeNumber g else 0x00)
    &   bitAt 6
    .~  (g ^. gpuLineCompareInterrupt)
    &   bitAt 5
    .~  (g ^. gpuOAMInterrupt)
    &   bitAt 4
    .~  (g ^. gpuVblankInterrupt)
    &   bitAt 3
    .~  (g ^. gpuHblankInterrupt)
    &   bitAt 2
    .~  gpuYAtCompare g
loadGPUControl 0xFF42 g = g ^. gpuScroll . _y
loadGPUControl 0xFF43 g = g ^. gpuScroll . _x
loadGPUControl 0xFF44 g = _gpuLine g
loadGPUControl 0xFF45 g = _gpuLineCompare g
loadGPUControl 0xFF46 g = _gpuDMAAddress g
loadGPUControl 0xFF47 g = getPalette $ _gpuBGPalette g -- non CBG mode only
loadGPUControl 0xFF48 g = getPalette $ _gpuOBJ0Palette g -- non CBG mode only
loadGPUControl 0xFF49 g = getPalette $ _gpuOBJ1Palette g -- non CBG mode only
loadGPUControl 0xFF4A g = g ^. gpuWindow . _y
loadGPUControl 0xFF4B g = g ^. gpuWindow . _x
loadGPUControl 0xff4d _ = 0xff
loadGPUControl addr _ =
  error $ printf "loadGPUControl: not in range 0x%04x" addr

module Memory.Accessible
  ( module Y
  , load8, store8, load16, store16
  )

where

import MonadEmulator as Y hiding (load8,load16,store8,store16)
import qualified MonadEmulator as X

import Data.Word
import Data.Bits

import Memory.OAM
import Memory.MMIO

alwaysLoadable :: Word16 -> Bool
alwaysLoadable addr = (addr < 0x8000) || (0xC000 <= addr && addr < 0xE000) || (0xFF00 <= addr)

alwaysStoreable :: Word16 -> Bool
alwaysStoreable addr = (0xC000 <= addr && addr < 0xE000) || (0xFF00 <= addr)

echoRam :: Word16 -> Bool
echoRam addr = 0xE000 <= addr && addr < 0xFE00

oam :: Word16 -> Bool
oam addr = 0xFE00 <= addr && addr < 0xFEA0

vram :: Word16 -> Bool
vram addr = 0x8000 <= addr && addr < 0xA000

-- externalRam :: Word16 -> Bool
-- externalRam addr = 0xA000 <= addr && addr < 0xC000

load8 :: MonadEmulator m => LoadStore8 -> m Word8
load8 x@(Addr8 addr)
  | alwaysLoadable addr = X.load8 x
  | echoRam addr        = X.load8 . Addr8 $ addr - 0x2000
  | vram addr           = do
      f <- (== 3) . (.&. 3) <$> X.load8 status
      if f then return 0xFF else X.load8 x
  | oam addr             = do
      f <- (`testBit` 1) <$> X.load8 status
      if f then return 0xFF else X.load8 x
  | otherwise = undefined
load8 x = X.load8 x

load16 :: MonadEmulator m => LoadStore16 -> m Word16
load16 x@(Addr16 addr)
  | alwaysLoadable addr = X.load16 x
  | echoRam addr = X.load16 . Addr16 $ addr - 0x2000
  | vram addr           = do
      f <- (== 3) . (.&. 3) <$> X.load8 status
      if f then return 0xFFFF else X.load16 x
  | oam addr             = do
      f <- (`testBit` 1) <$> X.load8 status
      if f then return 0xFFFF else X.load16 x
  | otherwise = undefined
load16 x = X.load16 x

store8 :: MonadEmulator m => LoadStore8 -> Word8 -> m ()
store8 x@(Addr8 addr)
  | alwaysStoreable addr = X.store8 x
  | echoRam addr = X.store8 . Addr8 $ addr - 0x2000
  | vram addr           = \b -> do
      f <- (== 3) . (.&. 3) <$> X.load8 status
      if f then return () else X.store8 x b
  | oam addr             = \b -> do
      f <- (`testBit` 1) <$> X.load8 status
      if f then return () else X.store8 x b
  | otherwise = undefined
store8 x = X.store8 x

store16 :: MonadEmulator m => LoadStore16 -> Word16 -> m ()
store16 x@(Addr16 addr)
  | alwaysStoreable addr = X.store16 x
  | echoRam addr = X.store16 . Addr16 $ addr - 0x2000
  | vram addr           = \w -> do
      f <- (== 3) . (.&. 3) <$> X.load8 status
      if f then return () else X.store16 x w
  | oam addr             = \w -> do
      f <- (`testBit` 1) <$> X.load8 status
      if f then return () else X.store16 x w
  | otherwise = undefined
store16 x = X.store16 x

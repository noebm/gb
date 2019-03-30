{- | Remapped memory for instructions -}
module Memory.Accessible
  ( module Y
  , load8, store8, load16, store16
  )

where

import MonadEmulator as Y hiding (load8,load16,store8,store16)
import qualified MonadEmulator as X

import Control.Monad (when)

import Text.Printf

import Data.Word
import Data.Bits

import Memory.MMIO


{-# INLINE alwaysLoadable #-}
alwaysLoadable :: Word16 -> Bool
alwaysLoadable addr = (addr < 0x8000) || (0xC000 <= addr && addr < 0xE000) || (0xFF00 <= addr)

{-# INLINE alwaysStoreable #-}
alwaysStoreable :: Word16 -> Bool
alwaysStoreable addr = (0xC000 <= addr && addr < 0xE000) || (0xFF00 <= addr)

{-# INLINE echoRam #-}
echoRam :: Word16 -> Bool
echoRam addr = 0xE000 <= addr && addr < 0xFE00

{-# INLINE oam #-}
oam :: Word16 -> Bool
oam addr = 0xFE00 <= addr && addr < 0xFEA0

{-# INLINE vram #-}
vram :: Word16 -> Bool
vram addr = 0x8000 <= addr && addr < 0xA000

-- externalRam :: Word16 -> Bool
-- externalRam addr = 0xA000 <= addr && addr < 0xC000

vramCheck, oamCheck :: MonadEmulator m => m Bool
vramCheck = (/= 3) . (.&. 3) <$> X.load8 status
oamCheck = not . (`testBit` 1) <$> X.load8 status

load8 :: MonadEmulator m => LoadStore8 -> m Word8
load8 x@(Addr8 addr)
  | alwaysLoadable addr = X.load8 x
  | echoRam addr        = X.load8 . Addr8 $ addr - 0x2000
  | vram addr           = tryAccess vramCheck
  | oam addr            = tryAccess oamCheck
  | otherwise            = error $ printf "load8 access to 0x%04x" addr
  where tryAccess check = do
          f <- check
          if f then X.load8 x else return 0xFF
load8 x = X.load8 x

load16 :: MonadEmulator m => LoadStore16 -> m Word16
load16 x@(Addr16 addr)
  | alwaysLoadable addr = X.load16 x
  | echoRam addr        = X.load16 . Addr16 $ addr - 0x2000
  | vram addr           = tryAccess vramCheck
  | oam addr            = tryAccess oamCheck
  | otherwise            = error $ printf "load16 access to 0x%04x" addr
  where tryAccess check = do
          f <- check
          if f then X.load16 x else return 0xFF
load16 x = X.load16 x

store8 :: MonadEmulator m => LoadStore8 -> Word8 -> m ()
store8 x@(Addr8 addr)
  | alwaysStoreable addr = X.store8 x
  | echoRam addr         = X.store8 . Addr8 $ addr - 0x2000
  | vram addr            = tryWrite vramCheck
  | oam addr             = tryWrite oamCheck
  | addr < 0x8000        = \_ -> return () -- ignore writes to cartridge
  | 0xFEA0 <= addr && addr < 0xFF00 = \_ -> return () -- ignore write s to unused memory area
  | otherwise            = \_ -> do
      r <- showRegisters
      error $ r ++ printf "\nstore8 access to 0x%04x" addr
  where tryWrite check b = do
          f <- check
          when f $ X.store8 x b
store8 x@(Register8 F) = X.store8 x . (.&. 0xF0)
store8 x = X.store8 x

store16 :: MonadEmulator m => LoadStore16 -> Word16 -> m ()
store16 x@(Addr16 addr)
  | alwaysStoreable addr = X.store16 x
  | echoRam addr         = X.store16 . Addr16 $ addr - 0x2000
  | vram addr            = tryWrite vramCheck
  | oam addr             = tryWrite oamCheck
  | otherwise            = error $ printf "store16 access to 0x%04x" addr
  where tryWrite check w = do
          f <- check
          when f $ X.store16 x w
store16 x@(Register16 AF) = X.store16 x . (.&. 0xFFF0)
store16 x = X.store16 x

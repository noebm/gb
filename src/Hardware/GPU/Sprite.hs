{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hardware.GPU.Sprite
  ( Sprite
  -- | Accessors for Sprite
  , spriteTile
  , spritePositionX, spritePositionY, spritePosition
  , spriteFlippedX, spriteFlippedY, spriteFlipped
  , spriteBGPriority
  , spriteDMGPalette

  -- | Object Access Memory
  , OAM
  , defaultOAM, dumpOAM
  , loadOAM, storeOAM
  , getLineSprites
  , directMemoryAccessOAM
  )
where

import qualified Data.Vector.Unboxed.Base as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

import Control.Monad
import Control.Applicative
import Data.List (sortBy)

import Control.Lens
import Data.Bits.Lens
import Data.Word
import SDL.Vect

import Data.Bits

import Hardware.GPU.GPUControl

data Sprite = Sprite
  { _spritePositionY' :: {-# UNPACK #-} !Word8
  , _spritePositionX' :: {-# UNPACK #-} !Word8
  , _spriteTile       :: {-# UNPACK #-} !Word8
  , _spriteAttributes :: {-# UNPACK #-} !Word8
  }

defaultSprite = Sprite 0x00 0x00 0x00 0x00

data instance U.Vector    Sprite =  V_Sprite {-# UNPACK #-} !Int !(U.Vector Word8)
data instance U.MVector s Sprite = MV_Sprite {-# UNPACK #-} !Int !(U.MVector s Word8)

instance U.Unbox Sprite

instance M.MVector U.MVector Sprite where
  basicLength (MV_Sprite n _) = n
  basicUnsafeSlice m n (MV_Sprite _ v) = MV_Sprite n (M.basicUnsafeSlice (4*m) (4*n) v)
  basicOverlaps (MV_Sprite _ v) (MV_Sprite _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_Sprite n) (M.basicUnsafeNew (4*n))
  basicUnsafeRead (MV_Sprite _ v) i = do
    let o = 4 * i
    a <- M.basicUnsafeRead v o
    b <- M.basicUnsafeRead v (o+1)
    c <- M.basicUnsafeRead v (o+2)
    d <- M.basicUnsafeRead v (o+3)
    return (Sprite a b c d)
  basicUnsafeWrite (MV_Sprite _ v) i (Sprite a b c d) = do
    let o = 4 * i
    M.basicUnsafeWrite v o     a
    M.basicUnsafeWrite v (o+1) b
    M.basicUnsafeWrite v (o+2) c
    M.basicUnsafeWrite v (o+3) d
  basicInitialize (MV_Sprite _ v) = M.basicInitialize v

instance G.Vector U.Vector Sprite where
  basicUnsafeFreeze (MV_Sprite n v) = liftM (V_Sprite n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_Sprite n v) = liftM (MV_Sprite n) (G.basicUnsafeThaw v)
  basicLength (V_Sprite n _) = n
  basicUnsafeSlice m n (V_Sprite _ v) = V_Sprite n (G.basicUnsafeSlice (4*m) (4*n) v)
  basicUnsafeIndexM (V_Sprite _ v) i = do
    let o = 4 * i
    a <- G.basicUnsafeIndexM v o
    b <- G.basicUnsafeIndexM v (o+1)
    c <- G.basicUnsafeIndexM v (o+2)
    d <- G.basicUnsafeIndexM v (o+3)
    return (Sprite a b c d)

makeLenses ''Sprite

spritePosition' :: Lens' Sprite (V2 Word8)
spritePosition' f (Sprite y x t attr) = (\(V2 x' y') -> Sprite y' x' t attr) <$> f (V2 x y)

spritePosition :: Lens' Sprite (V2 Word8)
spritePosition = spritePosition' . iso (subtract (V2 8 16)) (+ V2 8 16)

spritePositionX :: Lens' Sprite Word8
spritePositionX = spritePositionX' . iso (subtract 8) (+8)

spritePositionY :: Lens' Sprite Word8
spritePositionY = spritePositionY' . iso (subtract 16) (+16)

spriteFlippedX :: Lens' Sprite Bool
spriteFlippedX = spriteAttributes . bitAt 5

spriteFlippedY :: Lens' Sprite Bool
spriteFlippedY = spriteAttributes . bitAt 6

spriteFlipped :: Lens' Sprite (V2 Bool)
spriteFlipped f sprite
  = fmap (foldr ($) sprite . liftA2 set (V2 spriteFlippedX spriteFlippedY))
  $ f $ liftA2 view (V2 spriteFlippedX spriteFlippedY) (pure sprite)

spriteBGPriority :: Lens' Sprite Bool
spriteBGPriority = spriteAttributes . bitAt 7

-- XXX GBC palette (at bit 0 + 1) missing
spriteDMGPalette :: Lens' Sprite Bool
spriteDMGPalette = spriteAttributes . bitAt 4

spriteBytes :: Traversal' Sprite Word8
spriteBytes f (Sprite a b c d) = Sprite <$> f a <*> f b <*> f c <*> f d

newtype OAM = OAM (U.Vector Sprite)

defaultOAM :: OAM
defaultOAM = OAM (G.replicate 40 defaultSprite)

dumpOAM :: OAM -> [ Word8 ]
dumpOAM = G.toList . view oamBytesInternal

{-# INLINE oamBytesInternal #-}
oamBytesInternal :: Lens' OAM (U.Vector Word8)
oamBytesInternal f (OAM (V_Sprite n v)) = OAM . V_Sprite n <$> f v

loadOAM :: GPUControl -> OAM -> Word16 -> Maybe Word8
loadOAM GPUControl { _gpuMode = mode } oam addr = do
  guard (mode /= ModeVRAM || mode /= ModeOAM)
  G.indexM (oam ^. oamBytesInternal) $ fromIntegral (addr .&. 0x9f)

storeOAM :: GPUControl -> Word16 -> Word8 -> OAM -> Maybe OAM
storeOAM GPUControl { _gpuMode = mode } addr b oam = do
  guard (mode /= ModeVRAM || mode /= ModeOAM)
  return $ oam & oamBytesInternal.ix (fromIntegral (addr .&. 0x9f)) .~ b

{- |
  Returns all sprites for the current line.
  Sprites are ordered by draw priority, so that sprites with the highest priority are at the end.
-}
getLineSprites :: Word8 -> Word8 -> OAM -> U.Vector Sprite
getLineSprites line size (OAM oam)
  = G.backpermute collectedSprites spriteOrder

  where
    collectedSprites
      = G.take 10
      $ G.filter (\obj -> obj ^. spritePositionY' <= line + 16 && line + 16 < (obj ^. spritePositionY' + size))
      $ oam

    spriteOrder
      = G.fromList
      $ sortBy (\j i
                -> ((collectedSprites ^?! ix i.spritePositionX) `compare` (collectedSprites ^?! ix j.spritePositionX))
                 <> (i `compare` j))
      $ [0..G.length collectedSprites - 1]

directMemoryAccessOAM :: U.Vector Word8 -> OAM
directMemoryAccessOAM v
  | G.length v == 0xa0 = OAM (V_Sprite 40 v)
  | otherwise          = error $ "directMemoryAccessOAM: incorrect length - expected " ++ show 0xa0 ++ " but got " ++ show (G.length v)

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hardware.GPU.OAM
  ( Sprite
  -- | Accessors for Sprite
  , spriteTile
  , spritePositionX
  , spritePositionY
  , spritePosition
  , spriteFlippedX
  , spriteFlippedY
  , spriteFlipped
  , spriteBGPriority
  , spriteDMGPalette

  -- | Object Access Memory
  , OAM
  , defaultOAM
  , dumpOAM
  , loadOAM
  , storeOAM
  , getLineSprites
  , oamFromDMA
  , prettyOAM
  ) where

import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as M
import qualified Data.Vector.Unboxed.Base      as U

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Text.Printf

import           Control.Lens
import           Data.Bits.Lens
import           Data.Word
import           SDL.Vect

import           Data.Bits

data Sprite = Sprite
  { _spritePositionY' :: {-# UNPACK #-} !Word8
  , _spritePositionX' :: {-# UNPACK #-} !Word8
  , _spriteTile       :: {-# UNPACK #-} !Word8
  , _spriteAttributes :: {-# UNPACK #-} !Word8
  }

defaultSprite :: Sprite
defaultSprite = Sprite 0x00 0x00 0x00 0x00

data instance U.Vector    Sprite =  V_Sprite {-# UNPACK #-} !Int !(U.Vector Word8)
data instance U.MVector s Sprite = MV_Sprite {-# UNPACK #-} !Int !(U.MVector s Word8)

instance U.Unbox Sprite

instance M.MVector U.MVector Sprite where
  basicLength (MV_Sprite n _) = n
  basicUnsafeSlice m n (MV_Sprite _ v) =
    MV_Sprite n (M.basicUnsafeSlice (4 * m) (4 * n) v)
  basicOverlaps (MV_Sprite _ v) (MV_Sprite _ u) = M.basicOverlaps v u
  basicUnsafeNew n = fmap (MV_Sprite n) (M.basicUnsafeNew (4 * n))
  basicUnsafeRead (MV_Sprite _ v) i = do
    let o = 4 * i
    a <- M.basicUnsafeRead v o
    b <- M.basicUnsafeRead v (o + 1)
    c <- M.basicUnsafeRead v (o + 2)
    d <- M.basicUnsafeRead v (o + 3)
    return (Sprite a b c d)
  basicUnsafeWrite (MV_Sprite _ v) i (Sprite a b c d) = do
    let o = 4 * i
    M.basicUnsafeWrite v o a
    M.basicUnsafeWrite v (o + 1) b
    M.basicUnsafeWrite v (o + 2) c
    M.basicUnsafeWrite v (o + 3) d
  basicInitialize (MV_Sprite _ v) = M.basicInitialize v

instance G.Vector U.Vector Sprite where
  basicUnsafeFreeze (MV_Sprite n v) = fmap (V_Sprite n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_Sprite n v) = fmap (MV_Sprite n) (G.basicUnsafeThaw v)
  basicLength (V_Sprite n _) = n
  basicUnsafeSlice m n (V_Sprite _ v) =
    V_Sprite n (G.basicUnsafeSlice (4 * m) (4 * n) v)
  basicUnsafeIndexM (V_Sprite _ v) i = do
    let o = 4 * i
    a <- G.basicUnsafeIndexM v o
    b <- G.basicUnsafeIndexM v (o + 1)
    c <- G.basicUnsafeIndexM v (o + 2)
    d <- G.basicUnsafeIndexM v (o + 3)
    return (Sprite a b c d)

makeLenses ''Sprite

spritePosition' :: Lens' Sprite (V2 Word8)
spritePosition' f (Sprite y x t attr) =
  (\(V2 x' y') -> Sprite y' x' t attr) <$> f (V2 x y)

spritePosition :: Lens' Sprite (V2 Word8)
spritePosition = spritePosition' . iso (subtract (V2 8 16)) (+ V2 8 16)

spritePositionX :: Lens' Sprite Word8
spritePositionX = spritePositionX' . iso (subtract 8) (+ 8)

spritePositionY :: Lens' Sprite Word8
spritePositionY = spritePositionY' . iso (subtract 16) (+ 16)

spriteFlippedX :: Lens' Sprite Bool
spriteFlippedX = spriteAttributes . bitAt 5

spriteFlippedY :: Lens' Sprite Bool
spriteFlippedY = spriteAttributes . bitAt 6

spriteFlipped :: Lens' Sprite (V2 Bool)
spriteFlipped f sprite =
  fmap (foldr ($) sprite . liftA2 set (V2 spriteFlippedX spriteFlippedY))
    $ f
    $ liftA2 view (V2 spriteFlippedX spriteFlippedY) (pure sprite)

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

dumpOAM :: OAM -> [Word8]
dumpOAM = G.toList . view oamBytesInternal

prettyOAM :: OAM -> String
prettyOAM =
  unlines
    . fmap (intercalate ", " . fmap (printf "0x%02x"))
    . unfoldr (\xs -> guard (not (null xs)) *> Just (splitAt 16 xs))
    . G.toList
    . view oamBytesInternal

{-# INLINE oamBytesInternal #-}
oamBytesInternal :: Lens' OAM (U.Vector Word8)
oamBytesInternal f (OAM (V_Sprite n v)) = OAM . V_Sprite n <$> f v

loadOAM :: Word16 -> OAM -> Word8
loadOAM addr oam = oam ^?! oamBytesInternal . ix (fromIntegral (addr .&. 0xff))

storeOAM :: Word16 -> Word8 -> OAM -> OAM
storeOAM addr b = oamBytesInternal . ix (fromIntegral (addr .&. 0xff)) .~ b

{- |
  Returns all sprites for the current line.
  Sprites are ordered by draw priority, so that sprites with the highest priority are at the end.
-}
getLineSprites :: Word8 -> Word8 -> OAM -> U.Vector Sprite
getLineSprites line size (OAM oam) = G.backpermute collectedSprites spriteOrder

 where
  collectedSprites = G.take 10 $ G.filter
    (\obj ->
      obj
        ^. spritePositionY'
        <= line
        +  16
        && line
        +  16
        <  (obj ^. spritePositionY' + size)
    )
    oam

  spriteOrder = G.fromList $ sortBy
    (\j i ->
      (         (collectedSprites ^?! ix i . spritePositionX)
        `compare` (collectedSprites ^?! ix j . spritePositionX)
        )
        <> (i `compare` j)
    )
    [0 .. G.length collectedSprites - 1]

oamFromDMA :: Monad m => (Word16 -> m Word8) -> Word8 -> m OAM
oamFromDMA load base = OAM . V_Sprite 40 <$> G.generateM
  0xa0
  (load . fromIntegral . (baseAddr +))
  where baseAddr = fromIntegral base `shiftL` 8

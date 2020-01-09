{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GPU.Sprite where

import qualified Data.Vector.Unboxed.Base as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

import Control.Monad

import Control.Lens
import Data.Bits.Lens
import Data.Word

data Sprite = Sprite
  { _spritePositionY  :: {-# UNPACK #-} !Word8
  , _spritePositionX  :: {-# UNPACK #-} !Word8
  , _spriteTile       :: {-# UNPACK #-} !Word8
  , _spriteAttributes :: {-# UNPACK #-} !Word8
  }

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

spriteFlippedX :: Lens' Sprite Bool
spriteFlippedX = spriteAttributes . bitAt 5

spriteFlippedY :: Lens' Sprite Bool
spriteFlippedY = spriteAttributes . bitAt 6

spriteBGPriority :: Lens' Sprite Bool
spriteBGPriority = spriteAttributes . bitAt 7

spriteDMGPalette :: Lens' Sprite Bool
spriteDMGPalette = spriteAttributes . bitAt 4



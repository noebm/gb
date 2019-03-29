{- Object attribute memory -}
module Memory.OAM where

import SDL.Vect
import Data.Word
import Data.Bits
import Data.Maybe

import Control.Monad

import MonadEmulator
import Memory.MMIO

data ObjectAttribute = ObjectAttribute
  { objPosition   :: V2 Word8
  , objTile       :: Word8
  , objAttributes :: Word8
  }

objectAttribute :: MonadEmulator m => Word8 -> m (Maybe ObjectAttribute)
objectAttribute w = if w < 40 then do
    let baseAddr = 0xFE00
    let idx = baseAddr + fromIntegral w * 4
    y    <- load8 (Addr8   idx)
    x    <- load8 (Addr8 $ idx + 1)
    tile <- load8 (Addr8 $ idx + 2)
    attr <- load8 (Addr8 $ idx + 3)
    return $ Just $ ObjectAttribute (V2 x y) tile attr
    else return Nothing

{- remove maybes since we know that the address is correct -}
objectAttributes :: MonadEmulator m => m [ ObjectAttribute ]
objectAttributes = fmap fromJust <$> mapM objectAttribute [0..39]

{- find first 10 sprites that are in line -}
findSprites :: MonadEmulator m => Word8 -> Word8 -> m [ObjectAttribute]
findSprites ly h = take 10 . filter isVisibleOnLine <$> objectAttributes
  where isVisibleOnLine obj = let (V2 objx objy) = objPosition obj
                              in objx /= 0 && ly + 16 >= objy && ly + 16 < objy + h

accessibleOAM :: MonadEmulator m => m Bool
accessibleOAM = do
  mode <- (3 .&.) <$> load8 status
  return $ not $ mode `testBit` 1

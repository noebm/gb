{- Object attribute memory -}
module Memory.OAM where

import SDL.Vect
import Data.Word
import Data.Bits

import MonadEmulator
import Memory.MMIO

data ObjectAttribute = ObjectAttribute
  { objectPosition   :: V2 Word8
  , objectTile       :: Word8
  , objectAttributes :: Word8
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

accessibleOAM :: MonadEmulator m => m Bool
accessibleOAM = do
  mode <- (3 .&.) <$> load8 status
  return $ not $ mode `testBit` 1

module Drawing where

import Control.Monad.IO.Class
import Control.Monad

import Foreign.Ptr
import Foreign.Storable

import Data.Word
import Data.Bits

import SDL.Video hiding (paletteColor)
import SDL.Vect

import Memory.MMIO
import MonadEmulator

paletteColor :: Word8 -> Word8 -> Word8
-- paletteColor palette colorIndex = 0xFF - 0x3F * ((palette `shiftR` fromIntegral (2 * colorIndex)) .&. 0x3)
paletteColor palette colorIndex = case (palette `shiftR` fromIntegral (2 * colorIndex)) .&. 0x3 of
  0 -> 255
  1 -> 192
  2 -> 96
  3 -> 0
  _ -> error "impossible"

paletteValue :: Word8 -> Word8 -> Word8
paletteValue palette paletteIndex = palette `shiftR` fromIntegral (2 * paletteIndex) .&. 0x3

-- | Given the address of the tile and x / y pixel coordinates,
-- find the palette index of the pixel.
tile :: MonadEmulator m => Word16 -> Word8 -> Word8 -> m Word8
tile tileAddress y x = colorcode <$> load8 (Addr8 $ tileAddress + yOffset)
                                 <*> load8 (Addr8 $ tileAddress + yOffset + 1)
  where
  yOffset = fromIntegral ((y .&. 7) `shiftL` 1) -- every line contains 2 bytes
  xOffset = fromIntegral (complement x .&. 7)
  colorcode b0 b1
    = (if b1 `testBit` xOffset then (`setBit` 1) else id)
    $ (if b0 `testBit` xOffset then (`setBit` 0) else id)
    $ zeroBits

bgPalette :: MonadEmulator m => m (Word8 -> Word8)
bgPalette = paletteColor <$> load8 backgroundPalette

drawLineBackground :: (MonadIO m, MonadEmulator m) => LCDConfig -> m (Word8 -> Word8 -> m Word8)
drawLineBackground lcd = do
  bgrdPal <- bgPalette
  sy <- load8 scrollY
  sx <- load8 scrollX
  return $ \y x -> do
    let y' = sy + y
    let x' = sx + x
    idx <- load8 $ backgroundTileIndex lcd y' x'
    bgrdPal <$> tile (tileAddr lcd idx) y' x'
    -- (\x -> if x /= 0 then 255 else 0) <$> tile (tileAddr lcd idx) y' x'

genPixelRow :: (MonadIO m, MonadEmulator m) => Texture -> LCDConfig -> m ()
genPixelRow im conf = do
  y <- load8 currentLine
  fBgrd <- ($ y) <$> drawLineBackground conf
  (ptr', _) <- lockTexture im (Just $ fromIntegral <$> Rectangle (P $ V2 0 y) (V2 160 1))
  let ptr = castPtr ptr' :: Ptr Word8
  forM_ [0..159] $ \x -> do
    c <- fBgrd x
    let idx = 4 * fromIntegral x
    liftIO $ do
      poke (ptr `plusPtr` idx)       c
      poke (ptr `plusPtr` (idx + 1)) c
      poke (ptr `plusPtr` (idx + 2)) c
      poke (ptr `plusPtr` (idx + 3)) (0xFF :: Word8)
  unlockTexture im

{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module MMIO
--   ( writeMMIO
--   , accessMMIO
--   , MMIO
--   , defaultMMIO
--   , canAccessVRAM
--   , canAccessOAM
--   , updateGPU
--   )
where
import Control.Monad.IO.Class

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Bits
import Data.Bits.Lens

import Control.Lens
import Control.Monad.State
import Control.Monad

-- data Timer
-- data Audio
-- data Joypad

{-
data SweepDirection = SweepUp | SweepDown

data Sweep = Sweep
  { sweepTime :: Word8 -- ^ sweep time in k / 128 Hz with k in range [7..0]
  , sweepDirection :: SweepDirection
  , sweepShiftNumber :: Word8 -- ^ scaling number in range [7..0]
  }

data SoundChannel1 = SoundChannel1
  { sweep :: Maybe Sweep
  }

getSweep :: Word8 -> Maybe Sweep
getSweep w =
  if sweept == 0x00
  then Nothing
  else Just $ Sweep
       { sweepTime = sweept
       , sweepDirection = if w `testBit` 3 then SweepDown else SweepUp
       , sweepShiftNumber = w .&. 0x07
       }
  where sweept = (w `shiftR` 4) .&. 0x07

writeSweep :: Maybe Sweep -> Word8
writeSweep Nothing = 0x00
writeSweep (Just x)
  =   (sweepTime x `shiftL` 4)
  .|. (case sweepDirection x of SweepDown -> 0x08 ; SweepUp -> 0x00)
  .|. sweepShiftNumber x

applySweep :: Fractional a => Sweep -> a -> a
applySweep s x
  | SweepUp <- sweepDirection s = x * (1 + factor)
  | otherwise                   = x * (1 - factor)
  where factor = 1 / 2^ sweepShiftNumber s
-}

-- class Monad m => MonadGPU m where
--   updateLine :: m ()
--   updateScreen :: m ()

data MMIO = MMIO
  { _mmioData :: V.Vector Word8
  , _dotClock :: Word
  }

makeLenses ''MMIO

defaultMMIO :: MMIO
defaultMMIO = MMIO
  { _mmioData = V.replicate 0x80 0x00
  , _dotClock = 0
  }

interruptRequest :: Lens' MMIO Word8
interruptRequest = mmioData . singular (ix 0x0F)

statRequest :: Lens' MMIO Bool
statRequest = interruptRequest . bitAt 1

-- gpuMode :: MMIO -> Word8
-- gpuMode mmio = (mmioData mmio `B.index` 0x41) .&. 0x3

data GPUMode = OAMSearch | Transfer | HBlank | VBlank
  deriving Eq

{- R/W except for 2..0 bits of stat -}
lcdc, stat :: Lens' MMIO Word8
lcdc = mmioData . singular (ix 0x40)
stat = mmioData . singular (ix 0x41)

{- R/W -}
scx, scy :: Lens' MMIO Word8
scy = mmioData . singular (ix 0x42)
scx = mmioData . singular (ix 0x43)

{- read only -}
ly :: Lens' MMIO Word8
ly = mmioData .singular (ix 0x44)

lyc :: Lens' MMIO Word8
lyc = mmioData . singular (ix 0x45)

wy, wx :: Lens' MMIO Word8
wy = mmioData . singular (ix 0x4A)
-- offset by 7 (i.e. wx = 7 corresponds to corner)
wx = mmioData . singular (ix 0x4B)

{- R/W interrupt enable flags -}
statILY, statIOAM, statIVBLANK, statIHBLANK :: Lens' MMIO Bool
statILY = stat . bitAt 6
statIOAM = stat . bitAt 5
statIVBLANK = stat . bitAt 4
statIHBLANK = stat . bitAt 3

{- R - lyc == ly compare -}
statLY :: Lens' MMIO Bool
statLY = stat . bitAt 2

{- R - mode -}
statMode :: Lens' MMIO GPUMode
statMode = mmioData . singular (ix 0x41) . lens get' setter
   where
     get' x = case x .&. 0x3 of
       0 -> HBlank
       1 -> VBlank
       2 -> OAMSearch
       3 -> Transfer
       _ -> error "impossible"

     setter s x = (s .&. 0xFC) .|. case x of
       HBlank    -> 0
       VBlank    -> 1
       OAMSearch -> 2
       Transfer  -> 3

checkLY :: MonadState MMIO m => m ()
checkLY = do
  line  <- use ly
  lineCompare <- use lyc
  let cond = line == lineCompare
  statLY .= cond
  lyInterruptEnabled <- use statILY
  {- raise interrupt! -}
  when (lyInterruptEnabled && cond) $ statRequest .= True

accessMMIO :: MonadState MMIO m => Word16 -> m Word8
accessMMIO addr
  -- timer
  | 0xFF04 <= addr && addr <= 0xFF07 = return 0
  -- audio
  | 0xFF10 <= addr && addr <= 0xFF3F = return 0
  -- lcd controller
  | 0xFF40 <= addr && addr <= 0xFF6B = use (mmioData . singular (ix $ fromIntegral addr .&. 0x7F))
  | otherwise = error "mmio not implemented"

writeMMIO :: MonadState MMIO m => Word16 -> Word8 -> m ()
writeMMIO addr w
  -- mode bits are read only
  | addr == 0xFF41 = do
      w' <- use (mmioData . singular (ix addr'))
      assign (mmioData . singular (ix addr')) ((0xFA .&. w) .|. (0x5 .&. w'))
  -- ly clears on write
  | addr == 0xFF44 = assign (mmioData . singular (ix addr')) 0
  | otherwise = assign (mmioData . singular (ix addr')) w
  where addr' = fromIntegral $ addr .&. 0x7F
canAccessOAM :: MMIO -> Bool
canAccessOAM = views statMode (\s -> not $ s == OAMSearch || s == Transfer)

-- also cannot access palette data
canAccessVRAM :: MMIO -> Bool
canAccessVRAM = views statMode (/= Transfer)

-- 0xFF69 & 0xFF6B
canAccessCGBPalette :: MMIO -> Bool
canAccessCGBPalette = canAccessVRAM

updateGPU :: ({- MonadGPU m, -} MonadIO m, MonadState MMIO m) => Word -> m ()
updateGPU dt = do
  t <- dotClock <+= dt
  let next clocktime act = do
        let cond = t >= clocktime
        when cond $ do
          dotClock -= clocktime
          act
        return cond
  mode <- use statMode
  case mode of
    OAMSearch -> do
      liftIO $ putStrLn "OAMSearch"
      void $ next 80 (statMode .= Transfer)
      -- return False
    Transfer -> do
      liftIO $ putStrLn "Transfer"
      _f <- next 172 (statMode .= HBlank)
      return ()
      -- return True
      -- when f updateLine
    HBlank -> do
      liftIO $ putStrLn "HBLank"
      f <- next 204 $ ly += 1
      when f $ do
        l <- use ly
        statMode .= if l == 143 then VBlank else OAMSearch
      -- return False
        -- when (l == 143) updateScreen

    VBlank -> do
      -- liftIO $ putStrLn "VBLank"
      void $ next 456 $ do
        l <- ly <+= 1
        when (l > 153) $ do
          statMode .= OAMSearch
          ly .= 0
      -- return False
  -- checkLY

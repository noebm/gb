{-# LANGUAGE TemplateHaskell #-}
module Timer
  ( ClockSpeed(..)
  , Timer
  , TimerState
  , defaultTimerState
  , updateTimerState

  , inTimerRange
  , loadTimer
  , storeTimer
  )
where

import Control.Lens
import Data.Bits.Lens
import Data.Bits
import Data.Word

import Control.Monad.State

-- | Possible timer speeds relative to cpu frequency.
-- For example Clock16 means that the timer is running at 1 / 16 the speed of the cpu.
data ClockSpeed = Clock16 | Clock64 | Clock256 | Clock1024
  deriving (Show)

data TimerState = TimerState
  { _divider :: !Divider
  , _timer   :: !Timer
  , _clockSpeed :: !ClockSpeed
  }
  deriving (Show)

type Divider = Word16

data Timer = Timer
  { _counter :: Word8
  , _overflow :: Bool
  , _modulo  :: Word8
  , _enabled :: Bool
  }
  deriving (Show)

makeLenses ''Timer
makeLenses ''TimerState

defaultTimer :: Timer
defaultTimer = Timer
  { _counter = 0
  , _modulo = 0
  , _overflow = False
  , _enabled = False
  }

defaultTimerState :: TimerState
defaultTimerState = TimerState
  { _divider = 0
  , _timer = defaultTimer
  , _clockSpeed = Clock1024
  }

loadClockSpeed :: ClockSpeed -> Word8
loadClockSpeed c = case c of
  Clock16   -> 1
  Clock64   -> 2
  Clock256  -> 3
  Clock1024 -> 0

storeClockspeed :: Word8 -> ClockSpeed
storeClockspeed w = case w .&. 3 of
  1 -> Clock16
  2 -> Clock64
  3 -> Clock256
  0 -> Clock1024
  _ -> error "impossible"

{- |
   The divider is clocked at 2 ^ 14 Hz.
   Since the machine clock is 2 ^ 22 Hz
   the divider register need an update every 2 ^ 8 (= 256) cycles
-}

-- relevant bit of (2-byte) DIV for timer update
clockspeedBit :: ClockSpeed -> Int
clockspeedBit Clock16   = 3
clockspeedBit Clock64   = 5
clockspeedBit Clock256  = 7
clockspeedBit Clock1024 = 9

shouldIncreaseCounter :: State TimerState Bool
shouldIncreaseCounter = do
  d <- use divider
  bit <- uses clockSpeed clockspeedBit
  return $ d ^. bitAt bit

increaseCounter :: State Timer ()
increaseCounter = do
  c <- counter <<+= 1
  c' <- use counter
  overflow .= (c' < c)

tick :: State TimerState Bool
tick = do
  didOverflow <- use (timer.overflow)
  oldTIMABit  <- shouldIncreaseCounter
  isEnabled   <- use (timer.enabled)
  divider += 1
  -- delayed overflow
  when didOverflow $ do
    zoom timer $ do
      assign counter =<< use modulo
      overflow .= False
  unless didOverflow $ when (isEnabled && oldTIMABit) $ do
    newTIMABit <- shouldIncreaseCounter
    when (not newTIMABit) $ zoom timer $ increaseCounter
  return didOverflow

updateTimerState :: Word -> TimerState -> (Bool, TimerState)
updateTimerState bus_cycles = runState $
  any id <$> replicateM (fromIntegral bus_cycles) tick

inTimerRange :: Word16 -> Bool
inTimerRange addr = 0xff04 <= addr && addr < 0xff08

loadTimer :: TimerState -> Word16 -> Word8
loadTimer t 0xff04 = views divider (fromIntegral . (`shiftR` 6)) t
loadTimer t 0xff05 = view (timer.counter) t
loadTimer t 0xff06 = view (timer.modulo) t
loadTimer t 0xff07 = views clockSpeed loadClockSpeed t
                   & bitAt 2 .~ view (timer.enabled) t
loadTimer _ _ = error "loadTimer: not in range"

storeTimer :: Word16 -> Word8 -> TimerState -> TimerState
storeTimer 0xff04 _ = execState $ do
  oldTIMABit  <- shouldIncreaseCounter
  zoom timer increaseCounter
  divider .= 0x0000
storeTimer 0xff05 b = timer.counter .~ b
storeTimer 0xff06 b = timer.modulo .~ b
storeTimer 0xff07 b = \ t -> t
                    & timer.enabled .~ (b `testBit` 2)
                    & clockSpeed .~ storeClockspeed b
storeTimer _ _ = error "storeTimer: not in range"

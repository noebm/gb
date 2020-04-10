{-# LANGUAGE TemplateHaskell #-}
module Hardware.Timer
  ( ClockSpeed(..)
  , Timer
  , defaultTimer
  , updateTimer

  , inTimerRange
  , loadTimer
  , storeTimer
  )
where

import Control.Lens
import Data.Bits.Lens
import Data.Bits
import Data.Word

import Control.Monad.State.Strict

-- | Possible timer speeds relative to cpu frequency.
-- For example Clock16 means that the timer is running at 1 / 16 the speed of the cpu.
data ClockSpeed = Clock16 | Clock64 | Clock256 | Clock1024
  deriving (Show)

data Timer = Timer
  { _divider    :: {-# UNPACK #-} !Word16
  , _counter    :: {-# UNPACK #-} !Word8
  , _overflow   :: Bool
  , _modulo     :: Word8
  , _enabled    :: Bool
  , _clockSpeed :: ClockSpeed
  , __timerBit :: Word16 -> Bool
  }

makeLenses ''Timer

defaultTimer :: Timer
defaultTimer = Timer
  { _divider = 0
  , _counter = 0
  , _modulo = 0
  , _overflow = False
  , _enabled = False
  , _clockSpeed = Clock1024
  , __timerBit = const False
  }

updateTimerControl :: Word8 -> State Timer ()
updateTimerControl b = do
  let storeClockspeed w = case w .&. 3 of
        1 -> Clock16
        2 -> Clock64
        3 -> Clock256
        0 -> Clock1024
        _ -> error "impossible"
  let clockspeedBit Clock16   = 3
      clockspeedBit Clock64   = 5
      clockspeedBit Clock256  = 7
      clockspeedBit Clock1024 = 9

  enabled .= (b `testBit` 2)
  clockSpeed .= storeClockspeed b
  _timerBit .= \d -> (b `testBit` 2) && testBit d (clockspeedBit $ storeClockspeed b)

loadClockSpeed :: ClockSpeed -> Word8
loadClockSpeed c = case c of
  Clock16   -> 1
  Clock64   -> 2
  Clock256  -> 3
  Clock1024 -> 0

{- |
   The divider is clocked at 2 ^ 14 Hz.
   Since the machine clock is 2 ^ 22 Hz
   the divider register need an update every 2 ^ 8 (= 256) cycles
-}

{-# INLINE withFallingEdge #-}
withFallingEdge :: State Timer a -> State Timer a
withFallingEdge go = do
  d0 <- use _timerBit <*> use divider
  val <- go
  d1 <- use _timerBit <*> use divider
  when (d0 && not d1) increaseCounter
  return val

{-# INLINE increaseCounter #-}
increaseCounter :: State Timer ()
increaseCounter = do
  c <- counter <+= 1
  overflow .= (c == 0x00)

{-# INLINE tickdefault #-}
tickdefault :: State Timer ()
tickdefault = withFallingEdge $ divider += 1

{-# INLINE reset #-}
reset :: State Timer ()
reset = do
  divider += 1
  counter <~ use modulo
  overflow .= False

tick :: State Timer Bool
tick = do
  didOverflow <- use overflow
  if didOverflow then reset else tickdefault
  return didOverflow

{-# INLINE updateTimer #-}
updateTimer :: Word -> Timer -> (Bool, Timer)
updateTimer bus_cycles = runState $
  or <$> replicateM (fromIntegral bus_cycles) tick

inTimerRange :: Word16 -> Bool
inTimerRange addr = 0xff04 <= addr && addr < 0xff08

loadTimer :: Timer -> Word16 -> Word8
loadTimer t 0xff04 = views divider (fromIntegral . (`shiftR` 6)) t
loadTimer t 0xff05 = view counter t
loadTimer t 0xff06 = view modulo t
loadTimer t 0xff07 = views clockSpeed loadClockSpeed t
                   & bitAt 2 .~ view enabled t
loadTimer _ _ = error "loadTimer: not in range"

storeTimer :: Word16 -> Word8 -> Timer -> Timer
storeTimer 0xff04 _ = execState $ withFallingEdge $ divider .= 0
storeTimer 0xff05 b = counter .~ b
storeTimer 0xff06 b = modulo .~ b
storeTimer 0xff07 b = execState $ withFallingEdge $ updateTimerControl b

storeTimer _ _ = error "storeTimer: not in range"

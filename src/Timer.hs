{-# LANGUAGE TemplateHaskell #-}
module Timer
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
  { _divider    :: !Word16
  , _counter    :: Word8
  , _overflow   :: Bool
  , _modulo     :: Word8
  , _enabled    :: Bool
  , _clockSpeed :: ClockSpeed
  }
  deriving (Show)

makeLenses ''Timer

defaultTimer :: Timer
defaultTimer = Timer
  { _divider = 0
  , _counter = 0
  , _modulo = 0
  , _overflow = False
  , _enabled = False
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

shouldIncreaseCounter :: State Timer Bool
shouldIncreaseCounter = testBit <$> use divider <*> uses clockSpeed clockspeedBit

increaseCounter :: State Timer ()
increaseCounter = do
  c <- counter <<+= 1
  c' <- use counter
  overflow .= (c' < c)

{-# INLINE divBit #-}
divBit :: State Timer Bool
divBit = do
  d <- use divider
  b <- uses clockSpeed clockspeedBit
  e <- use enabled
  return $ testBit d b && e

{-# INLINE changeDIV #-}
changeDIV :: (Word16 -> Word16) -> State Timer Bool
changeDIV f = do
  d0 <- divBit
  divider %= f
  d1 <- divBit
  return $ d0 && not d1

changeTAC :: Word8 -> State Timer Bool
changeTAC b = do
  d0 <- divBit
  enabled .= (b `testBit` 2)
  clockSpeed .= storeClockspeed b
  d1 <- divBit
  return $ d0 && not d1

{-# INLINE tickdefault #-}
tickdefault :: State Timer ()
tickdefault = do
  fallingEdge <- changeDIV (+1)
  when fallingEdge increaseCounter

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
storeTimer 0xff04 _ = execState $ do
  fallingEdge <- changeDIV (\_ -> 0)
  when fallingEdge increaseCounter
storeTimer 0xff05 b = counter .~ b
storeTimer 0xff06 b = modulo .~ b
storeTimer 0xff07 b = execState $ do
  d0 <- divBit
  enabled .= (b `testBit` 2)
  clockSpeed .= storeClockspeed b
  d1 <- divBit
  when (d0 && not d1) increaseCounter
storeTimer _ _ = error "storeTimer: not in range"

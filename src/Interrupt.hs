module Interrupt
  ( Interrupt
  , enterInterrupt
  , handleInterrupt
  )
where

import MonadEmulator

import Data.Bits
import Data.Monoid

newtype Interrupt = Interrupt Int

enterInterrupt :: MonadEmulator m => Interrupt -> m Word
enterInterrupt (Interrupt k) = do
  setIEM False
  -- wait 2 cycles
  push =<< load16 (Register16 PC) -- 2 cycles
  let addr = 0x0040 + 8 * fromIntegral k
  store16 (Register16 PC) addr    -- 1 cycle
  return 20 -- 4 * 5

handleInterrupt :: MonadEmulator m => m (Maybe Interrupt)
handleInterrupt = do
  r <- getIEM
  if r
    then do
    interEnabled <- load8 (Addr8 0xFFFF)
    interFlags   <- load8 (Addr8 0xFF0F)
    let flags = interFlags .&. interEnabled
    -- get interrupt with highest priority
    let interrupt = getFirst $ mconcat $ First . Just <$> filter (flags `testBit`) [0..4]
    return $ fmap Interrupt interrupt
    else
    return Nothing

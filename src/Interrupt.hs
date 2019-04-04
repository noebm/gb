module Interrupt
  ( Interrupt
  , enterInterrupt
  , handleInterrupt
  )
where

import MonadEmulator
import Control.Monad

import Data.Bits
import Data.Monoid

import Memory.MMIO

newtype Interrupt = Interrupt Int

enterInterrupt :: MonadEmulator m => Interrupt -> m Word
enterInterrupt (Interrupt k) = do
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
    interEnabled <- load8 interruptEnable
    interFlags   <- load8 interruptFlag
    let flags = interFlags .&. interEnabled
    -- get interrupt with highest priority
    let interrupt = getFirst $ mconcat $ First . Just <$> filter (flags `testBit`) [0..4]
    forM interrupt $ \k -> do
      setIEM False
      store8 interruptFlag . (`clearBit` k) =<< load8 interruptFlag
      return $ Interrupt k
    else
    return Nothing

module Debugging where

import MonadEmulator
import Control.Monad.IO.Class
import Control.Monad

-- import Data.Word

import qualified Data.Vector.Unboxed.Mutable as V

clearRegisters :: MonadEmulator m => m ()
clearRegisters = do
  forM_ [A,F,B,C,D,E,H,L] $ \r -> store8 (Register8 r) 0x00
  forM_ [PC,SP] $ \r -> store16 (Register16 r) 0x0000

testRegisters :: (MonadEmulator m, MonadIO m) => m ()
testRegisters = do
  forM_ [A,F,B,C,D,E,H,L] $ \r -> do
    store8 (Register8 r) 0xFF
    liftIO . putStrLn =<< showRegisters
    store8 (Register8 r) 0x00
  forM_ [AF,BC,DE,HL,PC,SP] $ \r -> do
    store16 (Register16 r) 0xFF00
    liftIO . putStrLn =<< showRegisters
    store16 (Register16 r) 0x0000

-- dumpVRAM :: V.MVector s Word8 -> IO ()
dumpVRAM mem = liftIO $ do
  x <- forM [0x8000..0x9FFF] $ V.read mem
  print x

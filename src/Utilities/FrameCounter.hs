module Utilities.FrameCounter where

import           Data.IORef
import           Data.Word
import           System.Console.ANSI
import           Text.Printf
import           Utilities.Statistics.WindowedAverage

data FrameCounter delta = FrameCounter
  { updateCounter  :: delta -> IO ()
  , displayCounter :: IO ()
  }

setupFrameCounter :: IO (FrameCounter Word32)
setupFrameCounter = do
  avgWindowFrameTime  <- newIORef (emptyWindow 30 0)
  avgOverallFrameTime <- newIORef (emptyWindow 240 0)
  putStr "frame time:\n\n\n" -- setup terminal space
  return $ FrameCounter
    { updateCounter  = \dtime -> do
      modifyIORef avgWindowFrameTime
                  (addWindowSample (fromIntegral dtime :: Double))
      modifyIORef avgOverallFrameTime
                  (addWindowSample (fromIntegral dtime :: Double))
    , displayCounter = do
                         cursorUp 2
                         clearLine
                         putStrLn
                           .   (printf "30 frame window: %.2f ms")
                           .   averageWin
                           =<< readIORef avgWindowFrameTime
                         clearLine
                         putStrLn
                           .   (printf "240 frame window: %.2f ms")
                           .   averageWin
                           =<< readIORef avgOverallFrameTime
    }

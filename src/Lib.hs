{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad
import Control.Lens
import Control.Monad.IO.Class

import Data.IORef

import Graphics
import Hardware.HardwareMonad
import GB

import qualified SDL
import Utilities.SDL (_KeyboardEvent, _QuitEvent, _WindowClosedEvent)

import Instruction.Interpret

import Utilities.Step
import Utilities.Statistics.WindowedAverage
import Utilities.Statistics.IncrementalAverage

import System.Console.ANSI
import Text.Printf

-- faster than using Utilities.Step functions
steps :: Monad m => Word -> Step m a -> (a -> m b) -> m (Step m a)
steps 0 s _ = return s
steps n s f = do
  (dt, s') <- runStep s
  f dt
  steps (n - 1) s' f

-- setupCartridge :: Maybe FilePath -> Maybe FilePath -> IO (CartridgeState )
setupCartridge fpBoot fpRom = do
  let eitherError = either error id
  rom      <- eitherError <$> readRom fpRom
  bootrom' <- fmap eitherError <$> mapM readBootRom fpBoot
  return $ makeCartridge bootrom' rom

keymap :: SDL.Keycode -> Maybe Joypad
keymap SDL.KeycodeUp    = Just JoypadUp
keymap SDL.KeycodeDown  = Just JoypadDown
keymap SDL.KeycodeLeft  = Just JoypadLeft
keymap SDL.KeycodeRight = Just JoypadRight
keymap SDL.KeycodeZ = Just JoypadA
keymap SDL.KeycodeX = Just JoypadB
keymap SDL.KeycodeA = Just JoypadSelect
keymap SDL.KeycodeS = Just JoypadStart

keymap _ = Nothing

pressRelease :: SDL.InputMotion -> Bool -> Maybe Bool
pressRelease SDL.Pressed False = Just True
pressRelease SDL.Released _ = Just False
pressRelease _ _ = Nothing

updateKeys :: SDL.KeyboardEventData -> GB IO ()
updateKeys (SDL.KeyboardEventData _ press repeat keysym) =
  forM_ ((,) <$> (keymap $ SDL.keysymKeycode keysym) <*> pressRelease press repeat) updateJoypad

mainloop :: FilePath -> Bool -> IO ()
mainloop fp' nodelay = do

  let bootStrapName = "DMG_ROM.bin"
  cart <- setupCartridge (Just $ "./" ++ bootStrapName) fp'
  gfx <- initializeGraphics

  tickRef <- newIORef 0
  avgWindowFrameTime <- newIORef (emptyWindow 30 0)
  avgOverallFrameTime <- newIORef (IncrementalAverage 0 0)

  putStrLn "runtime statistics:"
  -- empty lines for updates
  putStrLn ""
  putStrLn ""

  runGB cart $ do

    let syncTimedHardware dt = do
          updateTimer dt
          frame <- updateGPU' dt

          forM_ frame $ \frame -> do
            generateImage (image gfx) frame
            renderGraphics gfx
            unless nodelay $ liftIO $ do
              told <- readIORef tickRef
              tnew <- SDL.ticks
              writeIORef tickRef tnew
              let dtime = tnew - told
              when (tnew > told && dtime < 16) $ SDL.delay (16 - dtime)

              modifyIORef avgWindowFrameTime (addWindowSample (fromIntegral dtime :: Double))
              modifyIORef avgOverallFrameTime (addSample (fromIntegral dtime :: Double))

              cursorUp 2
              clearLine
              putStrLn . (printf "current frame time: %.2f ms") . averageWin =<< readIORef avgWindowFrameTime
              clearLine
              putStrLn . (printf "overall frame time: %.2f ms") . average =<< readIORef avgOverallFrameTime

    let update s = do
          s' <- steps 100 s $ syncTimedHardware

          events <- fmap SDL.eventPayload <$> SDL.pollEvents
          mapMOf_ (folded . _KeyboardEvent) updateKeys events
          unless (has (folded . _QuitEvent) events || has (folded . _WindowClosedEvent) events) $ update s'

    update startExecution

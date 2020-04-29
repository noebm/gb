module Lib where

import Control.Monad
import Control.Monad.Except
import Control.Lens
import Data.IORef

import Graphics
import Input

import Hardware.HardwareMonad

import qualified SDL
import Utilities.SDL (_KeyboardEvent, _QuitEvent, _WindowClosedEvent)

import Utilities.Statistics.WindowedAverage

import System.Console.ANSI
import Text.Printf

import Emulate

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

basicSDLEmulationConfig :: Bool -> IO EmulationConfig
basicSDLEmulationConfig nodelay = do
  gfx <- initializeGraphics

  tickRef <- newIORef 0
  avgWindowFrameTime <- newIORef (emptyWindow 30 0)
  avgOverallFrameTime <- newIORef (emptyWindow 240 0)

  putStrLn "runtime statistics:"
  -- empty lines for updates
  putStrLn ""
  putStrLn ""

  return $ EmulationConfig
    { frameUpdate = \frame -> do
        renderFrame gfx frame

        -- frame time in ms
        told <- readIORef tickRef
        tnew <- SDL.ticks
        writeIORef tickRef tnew

        let dtime = tnew - told

        -- update terminal statistics output
        modifyIORef avgWindowFrameTime (addWindowSample (fromIntegral dtime :: Double))
        modifyIORef avgOverallFrameTime (addWindowSample (fromIntegral dtime :: Double))

        cursorUp 2
        clearLine
        putStrLn . (printf "current frame time: %.2f ms") . averageWin =<< readIORef avgWindowFrameTime
        clearLine
        putStrLn . (printf "overall frame time: %.2f ms") . averageWin =<< readIORef avgOverallFrameTime

        unless nodelay $ do
          when (tnew > told && dtime < 16) $ SDL.delay (16 - dtime)

    , keyUpdate = do
        events <- fmap SDL.eventPayload <$> SDL.pollEvents
        let keys = events ^.. folded . _KeyboardEvent . to (updateKeys keymap) . _Just
        let quit = has (folded . _QuitEvent) events || has (folded . _WindowClosedEvent) events
        return (quit, keys)
    , shouldSave = True
    }

mainloop :: FilePath -> Bool -> IO ()
mainloop fp nodelay = do

  let bootStrapName = "DMG_ROM.bin"
  (bootrom', rom) <- fmap (either error id) $ runExceptT $
    (,) <$> readBootRom bootStrapName <*> readRom fp

  emulate (Just bootrom') rom =<< basicSDLEmulationConfig nodelay

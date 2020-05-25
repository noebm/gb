module Lib where

import Control.Monad
import Control.Monad.Except
import Control.Lens
import Data.IORef
import Data.Serialize
import qualified Data.ByteString as B

import Graphics
import Input

import Hardware.HardwareMonad
import Hardware.Cartridge.Rom

import qualified SDL
import Utilities.SDL (_KeyboardEvent, _QuitEvent, _WindowClosedEvent)

import Emulate
import Utilities.FrameCounter

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

  counter <- setupFrameCounter

  return $ EmulationConfig
    { frameUpdate = \frame -> do
        renderFrame gfx frame

        -- frame time in ms
        told <- readIORef tickRef
        tnew <- SDL.ticks
        writeIORef tickRef tnew

        let dtime = tnew - told
        unless nodelay $ do
          when (tnew > told && dtime < 16) $ SDL.delay (16 - dtime)

        updateCounter counter dtime
        displayCounter counter

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
    (,) <$> readBootRom bootStrapName <*> readRom True fp

  config <- basicSDLEmulationConfig nodelay
  ramSave <- emulate (Just bootrom') rom config
  mapM_ (B.writeFile (romSaveFilePath rom) . encode) (guard (shouldSave config) *> ramSave)

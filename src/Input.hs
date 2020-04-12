module Input
  ( updateKeys
  )
where

import qualified SDL
import MonadEmulator
import Hardware.HardwareMonad

import Data.Foldable

type KeyMap = SDL.Keycode -> Maybe Joypad

pressRelease :: SDL.InputMotion -> Bool -> Maybe Bool
pressRelease SDL.Pressed False = Just True
pressRelease SDL.Released _ = Just False
pressRelease _ _ = Nothing

updateKeys :: KeyMap -> SDL.KeyboardEventData -> Emulator ()
updateKeys keymap (SDL.KeyboardEventData _ press repeat keysym) =
  forM_ ((,) <$> (keymap $ SDL.keysymKeycode keysym) <*> pressRelease press repeat) setJoypad

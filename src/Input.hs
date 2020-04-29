module Input
  ( updateKeys
  )
where

import qualified SDL
import Hardware.Joypad

type KeyMap = SDL.Keycode -> Maybe Joypad

pressRelease :: SDL.InputMotion -> Bool -> Maybe Bool
pressRelease SDL.Pressed False = Just True
pressRelease SDL.Released _ = Just False
pressRelease _ _ = Nothing

updateKeys :: KeyMap -> SDL.KeyboardEventData -> Maybe (Joypad, Bool)
updateKeys keymap (SDL.KeyboardEventData _ press rep keysym) =
  (,) <$> keymap (SDL.keysymKeycode keysym) <*> pressRelease press rep

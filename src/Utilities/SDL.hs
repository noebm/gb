module Utilities.SDL where

import Control.Lens
import qualified SDL

eventPayload :: Lens' SDL.Event SDL.EventPayload
eventPayload = lens SDL.eventPayload (\s x -> s { SDL.eventPayload = x })

_KeyboardEvent :: Prism' SDL.EventPayload SDL.KeyboardEventData
_KeyboardEvent = prism SDL.KeyboardEvent $ \x -> case x of
  SDL.KeyboardEvent e -> Right e
  _ -> Left x

{- SDL.KeyboardEventData -}
keyMotion :: Lens' SDL.KeyboardEventData SDL.InputMotion
keyMotion = lens SDL.keyboardEventKeyMotion (\s x -> s { SDL.keyboardEventKeyMotion = x })

keyRepeat :: Lens' SDL.KeyboardEventData Bool
keyRepeat = lens SDL.keyboardEventRepeat (\s x -> s { SDL.keyboardEventRepeat = x })

keyWindow :: Lens' SDL.KeyboardEventData (Maybe SDL.Window)
keyWindow = lens SDL.keyboardEventWindow (\s x -> s { SDL.keyboardEventWindow = x })

keyKeysym :: Lens' SDL.KeyboardEventData SDL.Keysym
keyKeysym = lens SDL.keyboardEventKeysym (\s x -> s { SDL.keyboardEventKeysym = x })

{- SDL.Keysym -}
keyScancode :: Lens' SDL.Keysym SDL.Scancode
keyScancode = lens SDL.keysymScancode (\s x -> s { SDL.keysymScancode = x })
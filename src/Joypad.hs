{-# LANGUAGE TemplateHaskell #-}
module Joypad
  ( JoypadState
  , defaultJoypadState

  , Joypad (..)
  , updateJoypad

  , inJoypadRange
  , loadJoypad
  , storeJoypad
  )
where

import qualified Data.Set as Set
import Control.Lens
import Data.Foldable
import Data.Word
import Data.Bits

data Joypad
  = JoypadLeft
  | JoypadRight
  | JoypadUp
  | JoypadDown

  | JoypadStart
  | JoypadSelect
  | JoypadA
  | JoypadB
  deriving (Enum, Ord, Eq, Show)

{-# INLINE joypadAll #-}
joypadAll :: Set.Set Joypad
joypadAll = Set.fromList $ enumFrom (toEnum 0)

{-# INLINE direction #-}
{-# INLINE button #-}
direction :: Joypad -> Bool
direction b = case b of
  JoypadLeft  -> True
  JoypadRight -> True
  JoypadUp    -> True
  JoypadDown  -> True
  _ -> False

button :: Joypad -> Bool
button = not . direction

joypadIndex :: Joypad -> Int
joypadIndex b = case b of
  JoypadDown   -> 3
  JoypadUp     -> 2
  JoypadLeft   -> 1
  JoypadRight  -> 0

  JoypadStart  -> 3
  JoypadSelect -> 2
  JoypadB      -> 1
  JoypadA      -> 0

data JoypadSelect = SelectDirection | SelectButton
  deriving (Show)

selected SelectButton = filtered button
selected SelectDirection = filtered direction

data JoypadState = JoypadState
  { _select :: Maybe JoypadSelect
  , _pressed :: Set.Set Joypad
  } deriving (Show)

makeLenses ''JoypadState

defaultJoypadState :: JoypadState
defaultJoypadState = JoypadState Nothing Set.empty

updateJoypad :: (Joypad -> Bool) -> JoypadState -> (JoypadState , Bool)
updateJoypad f s =
  let s' = s & pressed .~ Set.filter f joypadAll
      g t x = t ^.. pressed.folded.selected x
  in (,) s' $ maybe False (\x -> g s x == g s' x) (s' ^. select)

inJoypadRange :: Word16 -> Bool
inJoypadRange addr = addr == 0xff00

storeJoypad :: Word16 -> Word8 -> JoypadState -> JoypadState
storeJoypad 0xff00 b s
  | not (b `testBit` 4) = s & select ?~ SelectButton
  | not (b `testBit` 5) = s & select ?~ SelectDirection
  | otherwise = s

storeJoypad _ _ _ = error "storeJoypad: not in range"

loadJoypad :: JoypadState -> Word16 -> Word8
loadJoypad s 0xff00 = case s ^. select of
    Just SelectButton    -> foldl (.|.) 0x20 . fmap (bit . joypadIndex) $ filter button    $ toList (s ^. pressed)
    Just SelectDirection -> foldl (.|.) 0x10 . fmap (bit . joypadIndex) $ filter direction $ toList (s ^. pressed)
    _ -> 0x00
loadJoypad _ _ = error "loadJoypad: not in range"

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
  deriving (Enum, Ord, Eq)

joypadAll :: Set.Set Joypad
joypadAll = Set.fromList $ enumFrom (toEnum 0)

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

data JoypadState = JoypadState
  { _select :: JoypadSelect
  , _pressed :: Set.Set Joypad
  }

makeLenses ''JoypadState

defaultJoypadState :: JoypadState
defaultJoypadState = JoypadState SelectDirection Set.empty

updateJoypad :: (Joypad -> Bool) -> JoypadState -> JoypadState
updateJoypad f = pressed .~ Set.filter f joypadAll

inJoypadRange :: Word16 -> Bool
inJoypadRange addr = addr == 0xff00

storeJoypad :: Word16 -> Word8 -> JoypadState -> JoypadState
storeJoypad 0xff00 b s
  | not (b `testBit` 4) = s & select .~ SelectButton
  | not (b `testBit` 5) = s & select .~ SelectDirection
  | otherwise = s

storeJoypad _ _ _ = error "storeJoypad: not in range"

loadJoypad :: JoypadState -> Word16 -> Word8
loadJoypad s 0xff00 = foldl (.|.) 0x00 . fmap (bit . joypadIndex) $ case s ^. select of
    SelectButton    -> filter button $ toList (s ^. pressed)
    SelectDirection -> filter direction $ toList (s ^. pressed)
loadJoypad _ _ = error "loadJoypad: not in range"

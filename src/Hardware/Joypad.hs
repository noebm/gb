{-# LANGUAGE TemplateHaskell #-}
module Hardware.Joypad
  ( JoypadState
  , defaultJoypadState
  , Joypad(..)
  , updateJoypad
  , loadJoypad
  , storeJoypad
  ) where

import           Control.Lens
import           Data.Bits
import           Data.Foldable
import qualified Data.Set                      as Set
import           Data.Word

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

{-# INLINE direction #-}
{-# INLINE button #-}
direction :: Joypad -> Bool
direction b = case b of
  JoypadLeft  -> True
  JoypadRight -> True
  JoypadUp    -> True
  JoypadDown  -> True
  _           -> False

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

data JoypadState = JoypadState
  { _select  :: Maybe JoypadSelect
  , _pressed :: Set.Set Joypad
  }
  deriving Show

makeLenses ''JoypadState

defaultJoypadState :: JoypadState
defaultJoypadState = JoypadState Nothing Set.empty

updateJoypad :: (Joypad, Bool) -> JoypadState -> JoypadState
updateJoypad (joykey, True ) = pressed %~ Set.insert joykey
updateJoypad (joykey, False) = pressed %~ Set.delete joykey

storeJoypad :: Word8 -> JoypadState -> JoypadState
storeJoypad b | not (b `testBit` 5) = select ?~ SelectButton
              | not (b `testBit` 4) = select ?~ SelectDirection
              | otherwise           = id

loadJoypad :: JoypadState -> Word8
loadJoypad s = case s ^. select of
  Just SelectButton ->
    foldl xor 0xef . fmap (bit . joypadIndex) $ filter button $ toList
      (s ^. pressed)
  Just SelectDirection ->
    foldl xor 0xdf . fmap (bit . joypadIndex) $ filter direction $ toList
      (s ^. pressed)
  _ -> 0xcf

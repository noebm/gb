{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BinaryLiterals, NumericUnderscores #-}
-- | Implementation of serial port
-- Only master mode is currrently implemented.
-- This module does not emulate the bitshifting behaviour when transfering bytes.
module Hardware.Serial
  ( SerialConnection
  , SerialPort
  , defaultSerialPort
  , tickSerial
  , loadSerial
  , storeSerial
  ) where

import           Data.Bits
import           Data.Maybe
import           Data.Monoid
import           Data.Word

import           Control.Lens
import           Data.Bits.Lens

import           Control.Monad.State.Strict

type SerialConnection m = Word8 -> m Word8

data SerialPortTransfer = Active {-# UNPACK #-} !Word | Inactive

makePrisms ''SerialPortTransfer

data SerialPort = SerialPort
  { serialPortPayload  :: {-# UNPACK #-} !Word8
  , serialPortTransfer :: !SerialPortTransfer
  , serialPortMaster   :: !Bool
  }

makeLensesWith camelCaseFields ''SerialPort

defaultSerialPort :: SerialPort
defaultSerialPort = SerialPort 0x00 Inactive False

{-# INLINE _ActiveSum #-}
_ActiveSum :: Prism' SerialPortTransfer (Sum Word)
_ActiveSum = _Active . _Unwrapped

{-# INLINE tickSerial #-}
tickSerial
  :: Monad m
  => Maybe (SerialConnection m)
  -> Word
  -> SerialPort
  -> m (Bool, SerialPort)
tickSerial conn dt = runStateT $ do
  v <- transfer . _ActiveSum <+= fromIntegral dt
  let shouldTransfer = v >= 128
  when shouldTransfer $ do
    transfer .= Inactive
    forM_ conn $ \c -> do
      ret <- lift . c =<< use payload
      assign payload ret
  return (shouldTransfer && has _Just conn)

loadSerial :: Word16 -> SerialPort -> Word8
loadSerial 0xff01 s = view payload s
loadSerial 0xff02 s =
  0b0111_1110 & bitAt 0 .~ view master s & bitAt 7 .~ has (transfer . _Active) s
loadSerial _ _ = error "loadSerial: not in range"

storeSerial :: Word16 -> Word8 -> SerialPort -> SerialPort
storeSerial 0xff01 dat = payload .~ dat
storeSerial 0xff02 dat = execState $ do
  master .= (dat ^. bitAt 0)
  when (dat ^. bitAt 7) $ transfer .= Active 0
storeSerial _ _ = error "loadSerial: not in range"

module Emulate
  ( emulate
  , EmulationConfig(..)
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe                     ( isNothing )

import           Hardware.HardwareMonad
import           Instruction.Interpreter
import           MonadEmulator
import           Utilities.Cofree

data EmulationConfig = EmulationConfig
  { frameUpdate :: Frame -> IO ()
  , keyUpdate   :: IO (Bool, [(Joypad, Bool)])
  , shouldSave  :: Bool
  }

emulate
  :: Maybe BootRom -> Rom -> EmulationConfig -> IO (Maybe CartridgeRAMSave)
emulate brom rom conf = runEmulator (EmulatorConfig brom rom) $ do
  when (isNothing brom) $ storePC 0x100
  update =<< stepper
  saveEmulatorT
 where
  stepper =
    instructions >>= coExtend (tickHardware Nothing) >>= catMaybes >>= coExtend
      (liftIO . frameUpdate conf)
  update s = do
    s'           <- unwrap s
    (quit, keys) <- liftIO (keyUpdate conf)
    forM_ keys setJoypad
    unless quit $ update s'

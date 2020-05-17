module Emulate
  ( emulate
  , EmulationConfig (..)
  )
  where

import Control.Monad
import Control.Monad.IO.Class

import MonadEmulator
import Hardware.HardwareMonad
import Hardware.Cartridge.Rom
import Instruction.Interpreter
import Utilities.Cofree

import Data.Serialize
import qualified Data.ByteString as B

data EmulationConfig = EmulationConfig
  { frameUpdate :: Frame -> IO ()
  , keyUpdate :: IO (Bool , [ (Joypad, Bool) ])
  , shouldSave :: Bool
  }

emulate :: Maybe BootRom -> Rom -> EmulationConfig -> IO ()
emulate brom rom conf = do
  let stepper = instructions
        >>= coExtend (tickHardware Nothing)
        >>= coFilter
        >>= coExtend (liftIO . frameUpdate conf)
  let update s = do
        s' <- unwrap s
        (quit, keys) <- liftIO (keyUpdate conf)
        forM_ keys setJoypad
        unless quit $ update s'

  let inputConf = EmulatorConfig brom rom

  runEmulator inputConf $ do
    maybe (storePC 0x100) (\_ -> return ()) brom
    update =<< stepper

    when (shouldSave conf) $ do
      let fpSave = romSaveFilePath rom
      saveFile <- saveEmulatorT
      forM_ saveFile $ \dat -> liftIO $ do
        B.writeFile fpSave $ encode dat
        putStrLn $ "wrote save file to " ++ fpSave

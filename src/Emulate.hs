module Emulate
  ( emulate
  , EmulationConfig (..)
  )
  where

import Control.Monad
import Control.Monad.ST
import Control.Monad.IO.Class

import MonadEmulator
import Hardware.HardwareMonad
import Hardware.Cartridge.Rom
import Instruction.Interpreter

import Data.Word
import Data.Serialize
import qualified Data.ByteString as B

steps' :: Monad m => Word -> Cofree m a -> m (Cofree m a)
steps' 0 = return
steps' n = steps' (n - 1) <=< unwrap

extendM :: Monad m => (a -> m b) -> Cofree m a -> m (Cofree m b)
extendM f = go
  where go (x :< xs) = (:< (go =<< xs)) <$> f x

data EmulationConfig = EmulationConfig
  { frameUpdate :: Frame -> IO ()
  , keyUpdate :: IO (Bool , [ (Joypad, Bool) ])
  , shouldSave :: Bool
  }

emulate :: Maybe BootRom -> Rom -> EmulationConfig -> IO ()
emulate brom rom conf = do
  let sync = mapM_ (liftIO . frameUpdate conf) <=< tickHardware Nothing
  let stepper = extendM sync =<< instructions
  let update s = do
        s' <- steps' 100 s
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

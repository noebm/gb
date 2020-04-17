{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad
import Control.Lens hiding ((:<))
import Control.Monad.IO.Class
import Control.Monad.ST

import Data.IORef

import Graphics
import Input

import Hardware.HardwareMonad
import MonadEmulator

import qualified SDL
import Utilities.SDL (_KeyboardEvent, _QuitEvent, _WindowClosedEvent)

import Instruction.Interpret

import Utilities.Statistics.WindowedAverage

import System.Console.ANSI
import Text.Printf

steps' :: Monad m => Word -> Cofree m a -> m (Cofree m a)
steps' 0 = return
steps' n = steps' (n - 1) <=< unwrap

setupCartridge :: Maybe FilePath -> FilePath -> IO (CartridgeState RealWorld)
setupCartridge fpBoot fpRom = do
  let eitherError = either error id
  rom      <- eitherError <$> readRom fpRom
  bootrom' <- fmap eitherError <$> mapM readBootRom fpBoot
  stToIO $ makeCartridge bootrom' rom

keymap :: SDL.Keycode -> Maybe Joypad
keymap SDL.KeycodeUp    = Just JoypadUp
keymap SDL.KeycodeDown  = Just JoypadDown
keymap SDL.KeycodeLeft  = Just JoypadLeft
keymap SDL.KeycodeRight = Just JoypadRight
keymap SDL.KeycodeZ = Just JoypadA
keymap SDL.KeycodeX = Just JoypadB
keymap SDL.KeycodeA = Just JoypadSelect
keymap SDL.KeycodeS = Just JoypadStart

keymap _ = Nothing

extendM :: Monad m => (a -> m b) -> Cofree m a -> m (Cofree m b)
extendM f = go
  where go (x :< xs) = (:< (go =<< xs)) <$> f x

mainloop :: FilePath -> Bool -> IO ()
mainloop fp' nodelay = do

  let bootStrapName = "DMG_ROM.bin"
  cart <- setupCartridge (Just $ "./" ++ bootStrapName) fp'
  gfx <- initializeGraphics

  tickRef <- newIORef 0
  avgWindowFrameTime <- newIORef (emptyWindow 30 0)
  avgOverallFrameTime <- newIORef (emptyWindow 240 0)

  putStrLn "runtime statistics:"
  -- empty lines for updates
  putStrLn ""
  putStrLn ""

  runEmulator cart $ do

    let syncTimedHardware dt = do
          frame <- tickHardware dt

          forM_ frame $ \frame -> do
            renderFrame gfx frame

            -- frame time in ms
            (told, tnew) <- liftIO $ do
              told <- readIORef tickRef
              tnew <- SDL.ticks
              writeIORef tickRef tnew
              return (told, tnew)
            let dtime = tnew - told

            -- update terminal statistics output
            liftIO $ do
              modifyIORef avgWindowFrameTime (addWindowSample (fromIntegral dtime :: Double))
              modifyIORef avgOverallFrameTime (addWindowSample (fromIntegral dtime :: Double))

              cursorUp 2
              clearLine
              putStrLn . (printf "current frame time: %.2f ms") . averageWin =<< readIORef avgWindowFrameTime
              clearLine
              putStrLn . (printf "overall frame time: %.2f ms") . averageWin =<< readIORef avgOverallFrameTime

            unless nodelay $ liftIO $ do
              when (tnew > told && dtime < 16) $ SDL.delay (16 - dtime)

    let
      update :: Cofree Emulator () -> Emulator ()
      update s = do
          s' <- steps' 100 s

          events <- fmap SDL.eventPayload <$> SDL.pollEvents
          mapMOf_ (folded . _KeyboardEvent) (updateKeys keymap) events
          unless (has (folded . _QuitEvent) events || has (folded . _WindowClosedEvent) events) $ update s'

    let startStep = extendM syncTimedHardware =<< instructions

    update =<< startStep

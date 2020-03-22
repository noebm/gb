{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad
import Control.Lens
import Control.Monad.IO.Class

import Data.IORef

import Graphics
import Hardware.HardwareMonad
import GB

import qualified SDL
import Utilities.SDL (_KeyboardEvent, _QuitEvent, _WindowClosedEvent)

import Instruction.Interpret

import Utilities.Step

-- faster than using Utilities.Step functions
steps :: Monad m => Word -> Step m a -> (a -> m b) -> m (Step m a)
steps 0 s _ = return s
steps n s f = do
  (dt, s') <- runStep s
  f dt
  steps (n - 1) s' f

-- setupCartridge :: Maybe FilePath -> Maybe FilePath -> IO (CartridgeState )
setupCartridge fpBoot fpRom = do
  let eitherError = either error id
  rom      <- eitherError <$> readRom fpRom
  bootrom' <- fmap eitherError <$> mapM readBootRom fpBoot
  return $ makeCartridge bootrom' rom

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

pressRelease :: SDL.InputMotion -> Bool -> Maybe Bool
pressRelease SDL.Pressed False = Just True
pressRelease SDL.Released _ = Just False
pressRelease _ _ = Nothing

updateKeys :: SDL.KeyboardEventData -> GB IO ()
updateKeys (SDL.KeyboardEventData _ press repeat keysym) =
  forM_ ((,) <$> (keymap $ SDL.keysymKeycode keysym) <*> pressRelease press repeat) updateJoypad

mainloop :: FilePath -> Bool -> Bool -> Bool -> IO ()
mainloop fp' bgrd wnd nodelay = do

  let bootStrapName = "DMG_ROM.bin"
  cart <- setupCartridge (Just $ "./" ++ bootStrapName) fp'
  gfx <- initializeGraphics

  bgrdTilemapWindow <-
    if bgrd
    then Just <$> newWindow "background tilemap" (pure 256) (Just 1)
    else return Nothing
  wndTilemapWindow <-
    if wnd
    then Just <$> newWindow "window tilemap"     (pure 256) (Just 1)
    else return Nothing

  tickRef <- newIORef 0

  runGB cart $ do
    let
      -- logger :: Maybe (Word16 -> Instruction ArgWithData -> IO ())
      logger = Nothing
      -- logger = Just $ \addr i -> do
      --   when (addr > 0xFF) $ putStrLn $ printf "0x%04x: %s" addr (show i)

    let syncTimedHardware dt = do
          updateTimer dt
          updateGPU dt $ \gpu req -> case req of
            Draw    -> do
              renderGraphics gfx
              unless nodelay $ liftIO $ do
                told <- readIORef tickRef
                tnew <- SDL.ticks
                writeIORef tickRef tnew
                let dt = tnew - told
                when (tnew > told && dt < 16) $ SDL.delay (16 - dt)

              let conf = gpuConfig gpu
              mapM_ (drawTileMap (conf ^. gpuTileDataSelect) (conf ^. gpuBGTileMapSelect) gpu) bgrdTilemapWindow
              mapM_ (drawTileMap (conf ^. gpuTileDataSelect) (conf ^. gpuWindowTileMapSelect) gpu) wndTilemapWindow
            NewLine -> genPixelRow (image gfx) gpu

    let update s = do
          s' <- steps 100 s $ syncTimedHardware

          events <- fmap SDL.eventPayload <$> SDL.pollEvents
          mapMOf_ (folded . _KeyboardEvent) updateKeys events
          unless (has (folded . _QuitEvent) events || has (folded . _WindowClosedEvent) events) $ update s'

    update startExecution

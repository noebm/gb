module Lib where

import Control.Monad.IO.Class
import Control.Monad

import Graphics
import MonadEmulator
import HardwareMonad
import GB

import Instruction.Interpret
import Instruction.Instruction

import Interrupt.Interrupt
import Interrupt.InterruptType

import Cartridge.Cartridge
import Cartridge.BootRom
import GPU.GPUState
import Timer
import Joypad (Joypad(..))

import qualified SDL
import Utilities.SDL (_KeyboardEvent, _QuitEvent)

import Control.Lens

updateCPU :: (HardwareMonad m, MonadEmulator m) => m (Maybe Instruction, Word)
updateCPU = do
  halted <- halt
  if not halted then do
    ime <- getIEM
    int <- if ime
      then processInterrupts
      else return False
    i <- parseInstructionM byte
    dt <- interpretM i
    let dt' = if int then 20 + dt else dt
    return (Just i , dt')
  else do
    f <- processInterrupts
    if f
      then clearHalt >> return (Nothing, 20)
      else return (Nothing, 4)

updateGraphics :: (MonadIO m , HardwareMonad m) => GraphicsContext -> Word -> m ()
updateGraphics gfx cyc = updateGPU cyc $ \gpu req -> case req of
    Draw    -> renderGraphics gfx
    NewLine -> genPixelRow (image gfx) gpu

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

mainloop :: FilePath -> IO ()
mainloop fp' = do

  let bootStrapName = "DMG_ROM.bin"
  cart <- setupCartridge (Just $ "./" ++ bootStrapName) fp'

  runGB cart $ do
    let
      -- logger :: Maybe (Word16 -> Instruction ArgWithData -> IO ())
      logger = Nothing
      -- logger = Just $ \addr i -> do
      --   when (addr > 0xFF) $ putStrLn $ printf "0x%04x: %s" addr (show i)

    let update fx = do
          replicateM 100 $ do
            pc <- loadPC
            (i, dt) <- updateCPU
            forM_ logger $ \f -> liftIO $ f pc i
            updateGraphics fx dt
            updateTimer dt

          events <- fmap SDL.eventPayload <$> SDL.pollEvents
          let kbevent = events ^.. folded . _KeyboardEvent
          forM_ kbevent $ \key -> do
            updateKeys key
          unless (isn't _Empty $ events ^.. folded . _QuitEvent) $
            update fx

    gfx <- initializeGraphics
    update gfx

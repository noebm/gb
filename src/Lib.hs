{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Lib
where

import Data.Foldable
import Data.Word
import Data.Bits

import Text.Printf

import Control.Monad.IO.Class
import Control.Monad

import Graphics
import MonadEmulator
import GB

import Instruction.Interpret
import Instruction.Instruction
import Instruction.Disassembler
import Interrupt.Interrupt
import Interrupt.InterruptType

import Cartridge.Cartridge
import Cartridge.BootRom

import GPU.GPUState
import GPU.Drawing

import Joypad (Joypad(..))
import SDL.Input.Keyboard
import SDL.Input.Keyboard.Codes

import qualified SDL
import Data.Maybe
import Control.Applicative
import Control.Lens
import Data.Traversable

import Utilities.SDL
import qualified Data.Set as Set

joypadMapping :: Joypad -> Scancode
joypadMapping JoypadUp    = ScancodeUp
joypadMapping JoypadDown  = ScancodeDown
joypadMapping JoypadLeft  = ScancodeLeft
joypadMapping JoypadRight = ScancodeRight

joypadMapping JoypadA = ScancodeZ
joypadMapping JoypadB = ScancodeX
joypadMapping JoypadStart  = ScancodeReturn
joypadMapping JoypadSelect = ScancodeSpace

-- keyboardEvents :: MonadIO m => m ()
keyboardEvents s = do
  evs <- SDL.pollEvents <&> toListOf (traverse . eventPayload . _KeyboardEvent)
  let aux :: SDL.KeyboardEventData -> Set.Set Scancode -> Set.Set Scancode
      aux k = case k ^. keyMotion of
        SDL.Pressed  -> Set.insert sc
        SDL.Released -> Set.delete sc
        where sc = k ^. keyKeysym.keyScancode
  return $ foldrOf folded aux s evs

-- keyboardEvents :: MonadIO m => m ( Scancode -> Bool )
-- keyboardEvents = do
--   evs <- SDL.pollEvents
--   keys <- fmap catMaybes $ for evs $ \ev -> case SDL.eventPayload ev of
--     SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (Keysym k _ _)) -> return (Just k)
--     _ -> return Nothing
-- 
--   return $ \x -> x `elem` keys

updateJoypad s = do
  s' <- keyboardEvents s
  let f x = x `Set.member` s'
  -- liftIO . print =<< getJoypad
  changed <- updateJoypadGB $ f . joypadMapping
  when changed $ do
    i <- getInterrupt
    let iJOY = interruptJoypad i
    putInterrupt $ i { interruptJoypad = iJOY { interruptFlag = True }}
  return s'

updateCPU = do
  processInterrupts

  i <- parseInstructionM byte
  -- i <- disassemble
  dt <- interpretM i
  advCycles $ 4 * dt

  return i

gpuInterrupts :: MonadIO m => GPUState -> GB m ()
gpuInterrupts gpu = do
  let conf = gpuConfig gpu
  let lcdInterrupts = [ gpuOAMInterrupt, gpuHblankInterrupt, gpuYCompareInterrupt ]
  i <- getInterrupt
  when (any ($ conf) lcdInterrupts) $ do
    let iLCD = interruptLCD i
    putInterrupt $ i { interruptLCD = iLCD { interruptFlag = True } }
  when (gpuVblankInterrupt conf) $ do
    let iVBLK = interruptVBlank i
    putInterrupt $ i { interruptLCD = iVBLK { interruptFlag = True } }

updateGraphics gfx = updateGPU $ \gpu -> do
  gpuInterrupts gpu
  let conf = gpuConfig gpu
  case gpuMode conf of
    ModeVBlank | gpuYCoordinate conf == 144 -> renderGraphics gfx
    ModeHBlank -> genPixelRow (image gfx) gpu
    _ -> return ()

-- setupCartridge :: Maybe FilePath -> Maybe FilePath -> IO (CartridgeState )
setupCartridge fpBoot fpRom = do
  let eitherError = either error id
  rom      <- fmap eitherError <$> mapM readRom fpRom
  bootrom' <- fmap eitherError <$> mapM readBootRom fpBoot
  c <- mapM (makeCartridge bootrom') rom
  c' <- defaultCartridge
  return $ maybe c' id c

someFunc :: Maybe FilePath -> IO ()
someFunc fp' = do

  let bootStrapName = "DMG_ROM.bin"
  cart <- setupCartridge (Just $ "./" ++ bootStrapName) fp'

  runGB cart $ do
    let
      -- logger :: Maybe (Word16 -> Instruction ArgWithData -> IO ())
      logger = Nothing
      -- logger = Just $ \addr i -> do
      --   when (addr > 0xFF) $ putStrLn $ printf "0x%04x: %s" addr (show i)

    let update fx s = do
          -- forM_ [0..0] $ \_ -> do
          pc <- load16 (Register16 PC)
          i <- updateCPU
          forM_ logger $ \f -> liftIO $ f pc i
          updateGraphics fx
          updateJoypad s

    let runTillStop fx s0 = do
          s1 <- update fx s0
          s <- stop
          unless s $ runTillStop fx s1

    gfx <- initializeGraphics
    runTillStop gfx Set.empty

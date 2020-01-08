{-# LANGUAGE RankNTypes #-}
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
import Interrupt.Interrupt
import Interrupt.InterruptType

import Cartridge.Cartridge
import Cartridge.BootRom

import GPU.GPUState
import GPU.Drawing

import Timer
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

  {-
joypadMapping :: Joypad -> Scancode
joypadMapping JoypadUp    = ScancodeUp
joypadMapping JoypadDown  = ScancodeDown
joypadMapping JoypadLeft  = ScancodeLeft
joypadMapping JoypadRight = ScancodeRight

joypadMapping JoypadA = ScancodeZ
joypadMapping JoypadB = ScancodeX
joypadMapping JoypadStart  = ScancodeReturn
joypadMapping JoypadSelect = ScancodeSpace

keyboardEvents :: MonadIO m => Set.Set Scancode -> m (Set.Set Scancode)
keyboardEvents s = do
  evs <- SDL.pollEvents <&> toListOf (traverse . eventPayload . _KeyboardEvent)
  let aux :: SDL.KeyboardEventData -> Set.Set Scancode -> Set.Set Scancode
      aux k = case k ^. keyMotion of
        SDL.Pressed  -> Set.insert sc
        SDL.Released -> Set.delete sc
        where sc = k ^. keyKeysym.keyScancode
  return $ foldrOf folded aux s evs

updateJoypad s = do
  s' <- keyboardEvents s
  let f x = x `Set.member` s'
  -- liftIO . print =<< getJoypad
  changed <- updateJoypadGB $ f . joypadMapping
  when changed $
    modifyInterrupt $ interruptJoypad . interruptFlag .~ True
  return s'
-}

updateCPU :: MonadEmulator m => m (Maybe Instruction, Word)
updateCPU = do
  halted <- halt
  if not halted then do
    ime <- getIEM
    when ime $ void $ processInterrupts
    i <- parseInstructionM byte
    dt <- interpretM i
    return (Just i , dt)
  else do
    f <- processInterrupts
    if f
      then clearHalt >> return (Nothing, 0)
      else return (Nothing, 4)

updateGraphics :: (MonadIO m , MonadEmulator m) => GraphicsContext -> Word -> m ()
updateGraphics gfx cyc = updateGPU cyc $ \gpu -> do
  let conf = gpuConfig gpu
  when (_gpuVblankInterrupt conf) $ renderGraphics gfx
  when (_gpuHblankInterrupt conf) $ genPixelRow (image gfx) gpu

-- setupCartridge :: Maybe FilePath -> Maybe FilePath -> IO (CartridgeState )
setupCartridge fpBoot fpRom = do
  let eitherError = either error id
  rom      <- fmap eitherError <$> mapM readRom fpRom
  bootrom' <- fmap eitherError <$> mapM readBootRom fpBoot
  c <- mapM (makeCartridge bootrom') rom
  c' <- defaultCartridge
  return $ fromMaybe c' c

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
          pc <- loadPC
          t <- getCycles
          (i, dt') <- updateCPU
          advCycles dt'
          forM_ logger $ \f -> liftIO $ f pc i
          t' <- getCycles
          let dt = t' - t
          updateGraphics fx dt
          updateTimer dt
          -- updateJoypad s
          return s

    let runTillStop fx s0 = do
          s1 <- update fx s0
          s <- stop
          unless s $ runTillStop fx s1

    gfx <- initializeGraphics
    runTillStop gfx Set.empty

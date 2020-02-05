module Lib where

import Control.Monad.IO.Class
import Control.Monad

import Graphics
import MonadEmulator
import GB

import Instruction.Interpret
import Instruction.Instruction

import Cartridge.Cartridge
import Cartridge.BootRom
import GPU.GPUState
import Timer

updateCPU :: MonadEmulator m => m (Maybe Instruction, Word)
updateCPU = do
  halted <- halt
  if not halted then do
    ime <- getIEM
    when ime $ void processInterrupts
    i <- parseInstructionM byte
    dt <- interpretM i
    return (Just i , dt)
  else do
    f <- processInterrupts
    if f
      then clearHalt >> return (Nothing, 0)
      else return (Nothing, 4)

updateGraphics :: (MonadIO m , MonadEmulator m) => GraphicsContext -> Word -> m ()
updateGraphics gfx cyc = updateGPU cyc $ \gpu req -> case req of
    Draw    -> renderGraphics gfx
    NewLine -> genPixelRow (image gfx) gpu

-- setupCartridge :: Maybe FilePath -> Maybe FilePath -> IO (CartridgeState )
setupCartridge fpBoot fpRom = do
  let eitherError = either error id
  rom      <- eitherError <$> readRom fpRom
  bootrom' <- fmap eitherError <$> mapM readBootRom fpBoot
  makeCartridge bootrom' rom

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

    let runTillStop fx = do
          update fx
          s <- stop
          unless s $ runTillStop fx

    gfx <- initializeGraphics
    runTillStop gfx

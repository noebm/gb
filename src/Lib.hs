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
import Cartridge
import BootRom
import Memory.MMIO

import Interrupt
import Drawing

import Instruction.Interpret
import Instruction.Instruction
import Instruction.Disassembler

-- update :: (MonadEmulator m, MonadIO m) => GraphicsContext -> m (Bool , Instruction ArgWithData)
updateCPU = do
  mapM_ (advCycles <=< enterInterrupt) =<< handleInterrupt

  i <- parseInstructionM byte
  -- i <- disassemble
  interpretM i
  advCycles 20

  return i

handleGraphicsUpdate :: (MonadIO m, MonadEmulator m)
    => GraphicsContext -> LCDConfig -> Word8 -> m ()
handleGraphicsUpdate gfx conf = \case
  HBlank -> genPixelRow (image gfx) conf
  VBlank -> renderGraphics gfx
  _ -> return ()

updateGraphics :: (MonadEmulator m, MonadIO m)
               => GraphicsContext -> m ()
updateGraphics gfx = do
  lcd <- lcdConfig
  forM_ lcd $ \conf ->
    mapM_ (handleGraphicsUpdate gfx conf) =<< updateGPU

disableBootRom :: MonadEmulator m => m Bool
disableBootRom = (`testBit` 0) <$> load8 (Addr8 0xFF50)

someFunc :: Maybe FilePath -> IO ()
someFunc fp' = do
  rom <- readBootRom
  cart <- maybe (return emptyCartridge)
    (\fp -> do
        cartOrError <- loadCartridge fp
        return $ either error id cartOrError) fp'
  runGB cart $ do
    loadBootRom rom

    let
      -- logger :: Maybe (Word16 -> Instruction ArgWithData -> IO ())
      logger = Just $ \addr i -> do
        when (addr > 0xFF) $ putStrLn $ printf "0x%04x: %s" addr (show i)

    let update fx = do
          forM_ [0..0] $ \_ -> do
            pc <- load16 (Register16 PC)
            i <- updateCPU
            forM_ logger $ \f -> liftIO $ f pc i
          updateGraphics fx

    let runTillStop fx = do
          update fx
          s <- stop
          unless s $ runTillStop fx

    let bootStrap fx = do
          update fx

          bootflag <- disableBootRom
          s <- stop
          unless (s || bootflag) $ bootStrap fx


    gfx <- initializeGraphics
    bootStrap gfx
    unloadBootRom (cartridgeData cart)
    runTillStop gfx

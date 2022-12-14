{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import           Control.Monad.Except
import           Data.Word
import           Disassemble
import           Hardware.Cartridge
import           Lib
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Printf
import           Text.Read

data Options = Help
             | Info
             | NoDelay
             | Disassemble (Maybe Word16)
  deriving (Eq)

makePrisms ''Options

options =
  [ Option ['h'] ["help"] (NoArg Help) "Print this help message"
  , Option ['i'] ["info"] (NoArg Info) "Display rom information"
  , Option [] ["no-delay"] (NoArg NoDelay) "Disable frame delay"
  , Option ['d'] ["disassemble"] (OptArg disasm "address") "Disassemble rom"
  ]
  where disasm x = Disassemble $ readMaybe =<< x

parseCommandline progName argv = case getOpt Permute options argv of
  (opts, files, [])
    | Help `elem` opts -> do
      hPutStrLn stderr (usageInfo header options)
      exitSuccess
    | Info `elem` opts, [file] <- files -> do
      errors <- runExceptT $ do
        rom <- readRom False file
        liftIO $ print $ getRomHeader rom
      either (hPutStrLn stderr) return errors
      exitSuccess
    | Just optAddr <- firstOf (folded . _Disassemble) opts, [file] <- files -> do
      errors <- runExceptT $ do
        rom <- readRom False file
        liftIO $ putStrLn $ showAddressMap $ case optAddr of
          Nothing   -> disassemble rom
          Just addr -> disassembleAt rom addr
      either (hPutStrLn stderr) return errors
      exitSuccess
    | [file] <- files -> return (file, NoDelay `elem` opts)
  (_, _, errs) -> do
    hPutStrLn stderr (concat errs ++ usageInfo header options)
    exitWith (ExitFailure 1)
  where header = printf "Usage: %s [OPTION..] rom" progName

main :: IO ()
main = do
  args            <- getArgs
  progName        <- getProgName
  (file, nodelay) <- parseCommandline progName args
  mainloop file nodelay

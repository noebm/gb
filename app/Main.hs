module Main where

import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt

import Lib
import Cartridge.Cartridge

data Options = Help | Info | DebugWindow | DebugBackground
  deriving Eq

options =
  [ Option ['h'] ["help"] (NoArg Help) "Print this help message"
  , Option ['i'] ["info"] (NoArg Info) "Display rom information"
  , Option ['b'] ["background"] (NoArg DebugBackground) "Display window containing complete background map"
  , Option ['w'] ["window"]     (NoArg DebugWindow)     "Display window containing complete window map"
  ]

parseCommandline argv = case getOpt Permute options argv of
  (opts, files, [])
    | Help `elem` opts -> do
        hPutStrLn stderr (usageInfo header options)
        exitWith ExitSuccess
    | Info `elem` opts
    , [ file ] <- files -> do
        romOrError <- readRom file
        flip (either putStrLn) romOrError $ \(Rom h _) -> print h
        exitWith ExitSuccess
    | [ file ] <- files -> return (file , DebugBackground `elem` opts , DebugWindow `elem` opts)
  (_,_, errs) -> do
    hPutStrLn stderr (concat errs ++ usageInfo header options)
    exitWith (ExitFailure 1)
  where
    header = "Usage: <exec> [OPTION..] rom"

main :: IO ()
main = do
  (file, fbgrd, fwindow) <- parseCommandline =<< getArgs
  mainloop file fbgrd fwindow

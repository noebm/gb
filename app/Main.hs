module Main where

import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt
import Lib

data Options = Help
  deriving Eq

options =
  [ Option ['h'] ["help"] (NoArg Help) "Print this help message"
  ]

parseCommandline argv = case getOpt Permute options argv of
  (opts, files, [])
    | Help `elem` opts -> do
        hPutStrLn stderr (usageInfo header options)
        exitWith ExitSuccess
    | [ file ] <- files -> return file
  (_,_, errs) -> do
    hPutStrLn stderr (concat errs ++ usageInfo header options)
    exitWith (ExitFailure 1)
  where
    header = "Usage: <exec> [OPTION..] rom"

main :: IO ()
main = do
  file <- parseCommandline =<< getArgs
  someFunc (Just file)

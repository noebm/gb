module Main where

import Control.Monad.IO.Class
import System.Environment

import Instruction.Disassembler
-- import Cartridge
import GB
import Lib

runAsm :: Maybe FilePath -> IO ()
runAsm fp' = do
  cart <- setupCartridge Nothing fp'
  runGB cart $ do
    liftIO . mapM_ print =<< runDisassembler (>= 0x8000)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runAsm Nothing
    x : _ -> runAsm (Just x)

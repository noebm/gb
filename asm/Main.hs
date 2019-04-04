module Main where

import Control.Monad.IO.Class
import System.Environment

import Instruction.Disassembler
import Cartridge
import GB

runAsm :: Maybe FilePath -> IO ()
runAsm fp' = do
  cart <- maybe (return emptyCartridge)
    (\fp -> do
        cartOrError <- loadCartridge fp
        return $ either error id cartOrError) fp'
  runGB cart $ do
    liftIO . mapM_ print =<< runDisassembler (>= 0x8000)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runAsm Nothing
    x : _ -> runAsm (Just x)

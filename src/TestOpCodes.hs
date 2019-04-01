module TestOpCodes where

import OpCode
import qualified Data.ByteString as B

import Cartridge (memoryBootRom)

import Control.Monad.State
import Text.Printf
import Data.Word

runTest :: IO ()
runTest = do
  rom <- memoryBootRom
  (`evalStateT` (0 :: Int)) $ do
    let
      byte :: StateT Int IO Word8
      byte = do
          k <- state (\k -> (k , k + 1))
          return $ rom `B.index` k
    let printInstr = do
          pc <- get
          instr <- parseInstructionM byte
          liftIO $ putStrLn $ printf "0x%04x: %s" pc (show instr)
          let byteToSkip = sum . fmap argSize $ arguments instr
          modify' (+ byteToSkip)

    let loop = do
          idx <- get
          when (idx < 0xFF) (printInstr >> loop)
    loop

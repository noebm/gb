{-# LANGUAGE FlexibleContexts #-}
module TestOpCodes where

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed.Mutable as VUM

import Cartridge (memoryBootRom, emptyCartridge)
import GB
import Data.Foldable (find)
import Data.Maybe

import MonadEmulator
import Instruction.Instruction
import Instruction.Interpret
import Instruction.CodePath

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
          when (idx == 0xA8) (put 0xE0 >> liftIO (putStrLn "skipping data..."))
          when (idx < 0xFF) (printInstr >> loop)
    loop

runInterpretTest :: IO ()
runInterpretTest = do
  rom <- memoryBootRom
  let copyData bs = do
        mem <- unsafeMemory
        liftIO $ forM_ [0..B.length bs - 1] $ \idx ->
          VUM.write mem idx (bs `B.index` idx)
  runGB emptyCartridge $ do
    copyData rom
    let aux = do
          i <- parseInstructionM byte
          liftIO $ print i
          interpretM i

    let loop f = do
          idx <- load16 (Register16 PC)
          when (idx < 0xFF) (f >> loop f)
    loop aux

runInterpretTestOptimized :: IO ()
runInterpretTestOptimized = do
  rom <- memoryBootRom
  let copyData bs = do
        mem <- unsafeMemory
        liftIO $ forM_ [0..B.length bs - 1] $ \idx ->
          VUM.write mem idx (bs `B.index` idx)
  runGB emptyCartridge $ do
    copyData rom
    (`evalStateT` ([] :: [ CodePath (GB IO) ])) $ do
      let run addr = do
            hasCodePath <- gets (find (\cp -> entryAddress cp == addr))
            let withCodePath cp = do
                  liftIO $ putStrLn $ printf "executing path 0x%04x" (entryAddress cp)
                  executePath cp
                  return Nothing
            c <- StateT $ \s -> flip (,) s <$> maybe (getCodePath 3) withCodePath hasCodePath
            let updateCodePath cp = do
                  liftIO $ putStrLn $ printf "new path 0x%04x" (entryAddress cp)
                  liftIO $ print $ pathInstructions cp
                  modify' (cp :)
            mapM_ updateCodePath c
      let loop f = do
            pc <- load16 (Register16 PC)
            when (pc < 0xFF) (f pc >> loop f)
      loop run

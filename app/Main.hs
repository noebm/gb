module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> someFunc Nothing
    x : _ -> someFunc (Just x)

module Main where

import System.IO
import Parsing

main :: IO ()
main = do
     putStr "> "
     hFlush stdout
     line <- getLine
     putStrLn (show (primary line))
     main

module Main where

import Lib
import System.IO

main :: IO ()
main = do 
    inp <- getContents
    putStrLn $ chineseToHaskell inp

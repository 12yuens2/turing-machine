module Main where

import TuringMachineParser
import TuringMachine
import TuringTape

import Data.Maybe

import Data.List.Split

import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    content <- readFile $ args !! 0
    input   <- readFile $ args !! 1

    let tm      = createTM $ lines content
    let tp      = initTape (tail $ map createCell $ splitOn "" input) (alphabet $ fromJust tm)
        

    let result  = run (fromJust tm) $ fromJust tp
    putStrLn $ show result
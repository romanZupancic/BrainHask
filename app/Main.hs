module Main where

import System.Environment ( getArgs )

import CmdOptions(parseArguments)
import BfInterpreter(runBrainfuckProgram)
import BfState(showMemory)

main :: IO ()
main = do
    args <- getArgs
    let (optsRecord, nonops) = parseArguments args
    print optsRecord
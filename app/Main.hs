module Main where

import System.Environment ( getArgs )
import System.Console.GetOpt ( usageInfo )

import Control.Monad ( when )

import CmdOptions ( parseArguments, Options(..), optionDescriptions )
import BfInterpreter ( runBrainfuckProgram )
import BfState ( showMemory )

main :: IO ()
main = do
    args <- getArgs
    let (optsRecord, nonops) = parseArguments args
    case optsRecord of
        Options { optErrors = things@[_]} -> putStrLn $ concat things ++ "\nUse -h or --help for help."
        Options { optHelp = True } -> putStrLn usageDesc
        Options { optVersion = True} -> putStrLn "BrainHaskell 1.0"
        Options { optCmd = Just thing } -> runBfProgramWithOptions thing optsRecord
        -- TODO: Handle a bf source file as input
        _ -> putStrLn "Provide a Brainfuck source file to run."
    where
        usageDesc = usageInfo "Usage: bh [OPTION...] targetFile" optionDescriptions

runBfProgramWithOptions :: String -> Options -> IO ()
runBfProgramWithOptions instructions opts = do
    -- TODO: Handle interactive mode after program run
    state <- runBrainfuckProgram instructions
    Control.Monad.when (optMemoryOnExit opts) $
        putStrLn $ "\n\nFinal Memory State: \n" ++ showMemory state

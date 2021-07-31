module Main where

import System.Environment ( getArgs )
import System.Console.GetOpt ( usageInfo )

import Control.Monad ( when )

import CmdOptions ( parseArguments, Options(..), optionDescriptions )
import Repl ( brainfuckRepl )

import BfInterpreter ( runBrainfuckProgram )
import BfState ( showMemory )

main :: IO ()
main = do
    args <- getArgs
    let (optsRecord, nonOpts) = parseArguments args
    case optsRecord of
        Options { optErrors = Just errs} -> putStrLn $ concat errs ++ "\nUse -h or --help for help."
        Options { optHelp = True } -> putStrLn usageDesc
        Options { optVersion = True} -> putStrLn "BrainHaskell 1.0"
        Options { optCmd = Just thing } -> runBfProgramWithOptions thing optsRecord
        -- TODO: Handle a bf source file as input
        _ -> do
            instructions <- parseNonOpts nonOpts
            runBfProgramWithOptions instructions optsRecord
    where
        usageDesc = usageInfo "Usage: bh [OPTION...] [-c cmd | FILE | -]" optionDescriptions
        parseNonOpts nonOpts = if null nonOpts then return "" else readFile $ head nonOpts

runBfProgramWithOptions :: String -> Options -> IO ()
runBfProgramWithOptions instructions opts = do
    state <- runBrainfuckProgram instructions
    if optInteractive opts then do 
        interactiveState <- brainfuckRepl state
        finalMemory state
    else finalMemory state
    where 
        -- | If requested by opts, print final memory, else don't
        finalMemory state = Control.Monad.when (optMemoryOnExit opts) $
            putStrLn $ "\n\nFinal Memory State: \n" ++ showMemory state

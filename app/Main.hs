module Main where

import System.Environment ( getArgs )
import System.Console.GetOpt ( usageInfo )
import System.Exit ( exitFailure )
import Control.Monad ( when )
import Control.Exception ( try, IOException )

import CmdOptions ( parseArguments, Options(..), optionDescriptions )
import Repl ( startBrainfuckRepl )

import BfInterpreter ( runBrainfuckProgram )
import BfState ( showMemory )

main :: IO ()
main = do
    args <- getArgs
    let optsRecord = parseArguments args
    case optsRecord of
        Options { optErrors = Just errs} -> putStrLn $ concat errs ++ "\nUse -h or --help to view available options."
        Options { optHelp = True } -> putStrLn usageDesc
        Options { optVersion = True} -> putStrLn "BrainHask v1.0"
        Options { optCmd = Just instructions } -> runBfProgramWithOptions instructions optsRecord
        Options { optFile = Just filePath} -> do
            fileInstructions <- try $ readFile filePath :: IO (Either IOException String)
            case fileInstructions of
                Left except -> putStrLn ("File read error: " ++ show except) >> exitFailure
                Right instructions -> runBfProgramWithOptions instructions optsRecord
        -- Otherwise default behaviour
        _ -> runBfProgramWithOptions "" optsRecord
    where
        usageDesc = usageInfo "Usage: BrainHask [OPTION...] [-c cmd | FILE | -]" optionDescriptions

runBfProgramWithOptions :: String -> Options -> IO ()
runBfProgramWithOptions instructions opts = do
    state <- runBrainfuckProgram instructions
    if optInteractive opts then do 
        interactiveState <- startBrainfuckRepl state
        finalMemory state
    else finalMemory state
    where 
        -- | If requested by opts, print final memory, else don't
        finalMemory state = Control.Monad.when (optMemoryOnExit opts) $
            putStrLn $ "\n\nFinal Memory State: \n" ++ showMemory state

module Repl where

import System.IO ( stdout, hFlush )

import BfInterpreter ( processInstructions )
import BfState ( BrainfuckState
               , addInstructions
               , showMemory
               )

brainfuckRepl :: BrainfuckState -> IO BrainfuckState
brainfuckRepl state = do 
    read <- prompt
    case read of
        (':':xs) -> do
            case xs of
                "q" -> putStrLn "Quitting..."
                _ -> putStrLn $ xs ++ " is not a recognized command."
            brainfuckRepl state
        _ -> do
            let readState = addInstructions state read
            newState <- processInstructions readState
            putStrLn $ '\n' : showMemory newState
            brainfuckRepl newState

prompt :: IO String
prompt = do
    putStr "\n~~| "
    hFlush stdout
    getLine 
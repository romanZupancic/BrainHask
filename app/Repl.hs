module Repl where

import System.IO ( stdout, hFlush )
import Control.Monad ( when )

import BfInterpreter ( processInstructions )
import BfState ( BrainfuckState
               , addInstructions
               , showMemory
               )

data ReplState = ReplState { replBfState :: BrainfuckState
                           , replQuit :: Bool
                           , replToggleMemory :: Bool
                           } deriving (Show)

startBrainfuckRepl :: BrainfuckState -> IO BrainfuckState
startBrainfuckRepl bfState = do
    putStrLn "Brainhask v1.0.0\nType ':help' for more information, or ':quit' to quit"
    finalReplState <- brainfuckRepl ReplState { replBfState = bfState, replQuit = False, replToggleMemory = True }
    return $ replBfState finalReplState

brainfuckRepl :: ReplState -> IO ReplState
brainfuckRepl state@ReplState{ replBfState = bfState
                             } = do 
        -- TODO: Decide whether to evaluate previous state first, and then ask for commands (Evaluate - print - read - loop)
        read <- prompt
        newState <- processInput read state
        if replQuit newState then return state -- Quit the repl
        else do -- Print and then start a new repl iteration
            Control.Monad.when (replToggleMemory newState) $ putStrLn $ showMemory $ replBfState newState
            brainfuckRepl newState
    where
        processInput []    state = return state
        processInput input state = if head input == ':'
            -- Commands to change the behaviour of the repl / not BF programming
            then processReplCommand (tail input) state
            -- Apply the brainfuck programming
            else do
                newBfState <- processInstructions $ addInstructions (replBfState state) input
                return $ state { replBfState = newBfState }
        prompt = do
            putStr "\n~~|"
            hFlush stdout
            getLine

processReplCommand :: String -> ReplState -> IO ReplState
processReplCommand str state
    | str `elem` ["h", "help"] = do
        putStrLn $ formatHelp [ ("help", "Display this help message.")
                              , ("toggleMemory", "Toggle whether the repl displays Brainfuck state memory after each input.")
                              , ("quit", "Quit the Brainhask REPL.")
                              ]
        return state
    | str `elem` ["tm", "toggleMemory"] = do
        return $ state { replToggleMemory = not $ replToggleMemory state }
    | str `elem` ["q", "quit"] = do
        putStrLn "Quitting Brainfuck Repl..."
        return state { replQuit = True }
    | otherwise = do putStrLn $ str ++ ": Not a valid command."; return state
    where
        formatHelp commandDescriptions = case commandDescriptions of
            [] -> ""
            (cmd, desc):remaining -> cmd ++ "      " ++ desc ++
                                     if null remaining then ""          -- Don't add an extra "\n" if it's the last entry
                                     else "\n" ++ formatHelp remaining  -- Recurse on the remaining options
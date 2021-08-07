module Repl where

import System.IO ( stdout, hFlush )
import Control.Monad ( when )

import BfInterpreter ( processInstructions )
import BfState ( BrainfuckState (BrainfuckState, programLength)
               , addInstructions
               , showMemory
               )

data ReplState = ReplState { replBfState :: BrainfuckState
                           , replQuit :: Bool
                           , replToggleMemory :: Bool
                           } deriving (Show)

startBrainfuckRepl :: BrainfuckState -> IO BrainfuckState
startBrainfuckRepl bfState@BrainfuckState{ programLength = pl } = do
    if pl == 0 then putStrLn "\nBrainhask v1.0.0 REPL\nType ':help' for more information, or ':quit' to quit.\n" else putStrLn ""
    finalReplState <- brainfuckRepl ReplState { replBfState = bfState, replQuit = False, replToggleMemory = False } -- Set default states
    return $ replBfState finalReplState

brainfuckRepl :: ReplState -> IO ReplState
brainfuckRepl state@ReplState{ replBfState = bfState
                             , replQuit = rQuit
                             , replToggleMemory = rToggleMemory
                             } = if rQuit then return state -- Quit the repl
    else do -- Print and then start a new repl iteration
        Control.Monad.when rToggleMemory $ putStrLn $ "|Memory| " ++ showMemory bfState
        read <- prompt
        newState <- processInput read state
        brainfuckRepl newState
    where
        processInput []    state = return state
        processInput input state = if head input == ':'
            -- Commands to change the behaviour of the repl / not BF programming
            then processReplCommand (tail input) state
            -- Apply the brainfuck programming
            else do
                newBfState <- processInstructions $ addInstructions (replBfState state) input
                Control.Monad.when ('.' `elem` input) $ putStrLn "" -- Add a newline at the end of a sequence of instructions, only if those instructions actually prints something
                return state { replBfState = newBfState }
        prompt = do
            putStr "\n~~|"
            hFlush stdout
            getLine

processReplCommand :: String -> ReplState -> IO ReplState
processReplCommand str state
    | str `elem` ["h", "help"] = do
        putStrLn printHelp
        return state
    | str `elem` ["tm", "toggleMemory"] = do
        return $ state { replToggleMemory = not $ replToggleMemory state }
    | str `elem` ["q", "quit"] = do
        putStrLn "Quitting Brainfuck Repl..."
        return state { replQuit = True }
    | otherwise = do putStrLn $ str ++ ": Not a valid command."; return state
    where
        printHelp = "This REPL supports a series of short-form and long-form commands.\nAll commands require ':' in front of them.\n\n" ++
            formatHelp [ ("help", "h", "Display this help message.")
                       , ("toggleMemory", "tm", "Toggle whether the repl displays Brainfuck state memory after each input.")
                       , ("quit", "q", "Quit the Brainhask REPL.")
                       ] ++ "\n"
        formatHelp commandDescriptions = case commandDescriptions of
            [] -> ""
            (cmd, shrt, desc):remaining -> "    " ++ wordAndSpace cmd 15 ++ 
                                     wordAndSpace shrt 4 ++ desc ++
                                     if null remaining then ""          -- Don't add an extra "\n" if it's the last entry
                                     else "\n" ++ formatHelp remaining  -- Recurse on the remaining options

wordAndSpace :: String -> Int -> String
wordAndSpace str len = str ++ replicate (max 1 (len - length str)) ' '
module Lib
    ( readInstructions
    ) where

import Control.Monad (foldM)

-- | The data `BrainfuckState` encapsultes the entire brainfuck state: memory and the current pointer index
data BrainfuckState = BrainfuckState { prevMemory :: [Int]  -- All memory before the index pointer
                                     , currMemory :: Int    -- The memory at the index pointer
                                     , restMemory :: [Int]  -- All memory after the index pointer
                                     } deriving (Show)

readInstructions :: IO ()
readInstructions = do
    putStrLn "Nope"
    input <- getLine
    runBrainfuckProgram "+++." 
    putStrLn "End"

runBrainfuckProgram :: String -> IO BrainfuckState
runBrainfuckProgram program = processInstructions program BrainfuckState { prevMemory = [], currMemory = 0, restMemory = [0] }

processInstructions :: String -> BrainfuckState -> IO BrainfuckState
processInstructions [] state = return state
processInstructions xs state = foldM (flip processInstruction) state xs

-- | Each call of processInstructions computes a single Brainfuck instruction,
-- | and then recursively calls itself on remaining instructions
processInstruction :: Char -> BrainfuckState -> IO BrainfuckState
-- Compute memory manipulation
processInstruction '+' state@BrainfuckState{currMemory = c} = return state { currMemory = c + 1 }
processInstruction '-' state@BrainfuckState{currMemory = c} = return state { currMemory = c - 1 }

-- Compute I/O
processInstruction '.' state@BrainfuckState{currMemory = c} = do
    putStr $ show c
    return state
-- TODO: Make this work
processInstruction ',' state = return state

-- Compute index movements
processInstruction '<' state@(BrainfuckState [] _ _) = return state
processInstruction '<' (BrainfuckState ps c r) = return BrainfuckState {prevMemory = init ps, currMemory = last ps, restMemory = c:r}
processInstruction '>' (BrainfuckState p c []) = return BrainfuckState {prevMemory = p ++ [c], currMemory = 0, restMemory = []}
processInstruction '>' (BrainfuckState p c (r:rs)) = return BrainfuckState {prevMemory = p ++ [c], currMemory = r, restMemory = rs}

-- Compute loops
-- TODO: Make this work
processInstruction '[' state = return state
processInstruction ']' state = return state
processInstruction _ state = return state

replaceAtIndex :: [a] -> a -> Int -> [a]
replaceAtIndex [] _ _ = []
replaceAtIndex (x:xs) item index
    | index == 0 = item:xs
    | otherwise = x:replaceAtIndex xs item (index - 1)

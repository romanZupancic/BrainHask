module BfInterpreter
    ( runBrainfuckProgram
    , processInstructions
    ) where

import System.IO ( stdout, hFlush )
import Data.Char ( ord )
import Text.Read ( readMaybe )

import BfState ( BrainfuckState(..)
             , generateProgramStartState
             )

-- | Run the Brainfuck program with a default (all-zero) state
runBrainfuckProgram :: String -> IO BrainfuckState
runBrainfuckProgram instructions = processInstructions $ generateProgramStartState instructions

-- | Run all instructions in sequence
processInstructions :: BrainfuckState -> IO BrainfuckState
processInstructions state@BrainfuckState{program = p, programCounter = pc, programLength = pl} 
    | pl == pc = do     -- At the end of the program, return just it's state
        return state
    | otherwise = do    -- Run the program one instruction at a time
        newState <- processInstruction (p !! pc) state
        processInstructions newState {programCounter = programCounter newState + 1}

-- | Each call of processInstructions computes a single Brainfuck instruction,
-- | and then recursively calls itself on remaining instructions
processInstruction :: Char -> BrainfuckState -> IO BrainfuckState
-- Compute memory manipulation
processInstruction '+' state@BrainfuckState{currMemory = c} = return state { currMemory = c + 1 }
processInstruction '-' state@BrainfuckState{currMemory = c} = return state { currMemory = c - 1 }

-- Compute I/O
processInstruction '.' state@BrainfuckState{currMemory = c} = do
    putStr $ show c
    hFlush stdout
    return state
processInstruction ',' state = do
    input <- getLine
    case (readMaybe input :: Maybe Int) of
        Nothing ->  return state { currMemory = if null input then 0 else ord $ head input }
        Just inp -> return state { currMemory = inp }
    

-- Compute index movements
processInstruction '<' state@BrainfuckState{prevMemory=[]} 
    = return state     -- Don't move if already at start of memory
processInstruction '<' state@BrainfuckState{prevMemory=ps, currMemory=c, restMemory=r}
    = return state {prevMemory = init ps, currMemory = last ps, restMemory = c:r}
processInstruction '>' state@BrainfuckState{prevMemory=p, currMemory=c, restMemory=[]}
    = return state {prevMemory = p ++ [c], currMemory = 0, restMemory = []} -- Add new memory if at end of memory
processInstruction '>' state@BrainfuckState{prevMemory=p, currMemory=c, restMemory=(r:rs)}
    = return state {prevMemory = p ++ [c], currMemory = r, restMemory = rs} 

-- Compute loops
processInstruction '[' state@BrainfuckState{currMemory = c, program = instructions, programCounter = pc} 
    | c /= 0 = return state
    | otherwise = return state {programCounter = getMatchingIndex '[' ']' 1 (drop (pc + 1) instructions) + pc}
processInstruction ']' state@BrainfuckState{currMemory = c, program = instructions, programCounter = pc}
    | c == 0 = return state
    | otherwise = return state {programCounter = pc - 1 - getMatchingIndex ']' '[' 1 (reverse $ take pc instructions)}

-- All other characters are comments and do not change state
processInstruction _ state = return state

-- | Return the index of the matching pair of brackets
getMatchingIndex :: Eq a => a -- The designator for an open pair
    -> a -- The designator for the closed pair
    -> Int -- The number of encounters seen so far
    -> [a] -- The list to iterate through to scan for pairs
    -> Int -- The index in the given list that the closing pair can be found at
getMatchingIndex openValue closeValue encounters (x:xs)
    | x == openValue = 1 + getMatchingIndex openValue closeValue (encounters + 1) xs
    | x == closeValue = if encounters - 1 == 0 then 0 else 1 + getMatchingIndex openValue closeValue (encounters - 1) xs
    | otherwise = 1 + getMatchingIndex openValue closeValue encounters xs
getMatchingIndex _ _ _ [] = -1

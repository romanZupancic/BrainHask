module Main where

import BfInterpreter(runBrainfuckProgram)
import BfState(showMemory)

main :: IO ()
main = do
    putStrLn "Program start."
    finalState <- runBrainfuckProgram ",>,[<+>-]<.>>>>+++"
    putStrLn ""
    putStrLn $ "Final memory layout: " ++ showMemory finalState

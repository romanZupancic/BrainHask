module Lib
    ( readInstructions
    ) where

readInstructions :: IO ()
readInstructions = do
    putStrLn "Nope"
    input <- getLine 
    processInstructions input [0] 0
    putStrLn "End"

-- | Each call of processInstructions computes a single Brainfuck instruction,
-- | and then recursively calls itself on remaining instructions
processInstructions :: String -> [Int] -> Int -> IO [Int]
-- Compute memory manipulation
processInstructions ('+':xs) memory index = processInstructions xs newMemory index
    where
        newMemory = replaceIndex memory (memory !! index + 1) index
processInstructions ('-':xs) memory index = processInstructions xs newMemory index
    where
        newMemory = replaceIndex memory (memory !! index - 1) index

-- Compute I/O
processInstructions ('.':xs) memory index = do 
    putStr $ show (memory !! index)
    processInstructions xs memory index
processInstructions (',':xs) memory index = return [2]

-- Compute index movements
processInstructions ('<':xs) memory index = processInstructions xs memory (index - 1)
processInstructions ('>':xs) memory index = processInstructions xs memory (index + 1)

-- Compute loops
processInstructions ('[':xs) memory index = return [2]
processInstructions (']':xs) memory index = return [2]
processInstructions _ memory _ = return memory 

replaceIndex :: [a] -> a -> Int -> [a]
replaceIndex [] _ _ = []
replaceIndex (x:xs) item index
    | index == 0 = item:xs
    | otherwise = x:replaceIndex xs item (index - 1)

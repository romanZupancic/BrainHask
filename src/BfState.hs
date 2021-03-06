module BfState 
    ( BrainfuckState(..)
    , generateProgramStartState
    , showMemory
    , addInstructions
    ) where

-- | The data `BrainfuckState` encapsultes the entire brainfuck state: memory and the current pointer index
data BrainfuckState = BrainfuckState { prevMemory :: [Int]  -- All memory before the index pointer
                                     , currMemory :: Int    -- The memory at the index pointer
                                     , restMemory :: [Int]  -- All memory after the index pointer
                                     , program :: String
                                     , programCounter :: Int
                                     , programLength :: Int
                                     } deriving (Show, Eq)

-- | Generate a start state of a Brainfuck program using the given instructions as a base and setting
-- | All other necessary properties to zero
generateProgramStartState :: String -> BrainfuckState
generateProgramStartState instructions = BrainfuckState { prevMemory = []
                                                        , currMemory = 0
                                                        , restMemory = []
                                                        , program = instructions
                                                        , programCounter = 0
                                                        , programLength = length instructions
                                                        }

-- | Return a modification of the given Brainfuck state with the additional instructions inserted
addInstructions :: BrainfuckState -> String -> BrainfuckState
addInstructions state@BrainfuckState{program = p, programLength = pl} instructions = 
    state { program = p ++ instructions
          , programLength = pl + length instructions}

showMemory :: BrainfuckState -> String
showMemory BrainfuckState{prevMemory = p, currMemory = c, restMemory = r}
    = show $ p ++ [c] ++ r
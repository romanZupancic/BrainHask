import Test.Hspec
import Test.Hspec.Runner (defaultConfig, configFastFail, hspecWith)

import BfState ( BrainfuckState(..)
               , generateProgramStartState
               )
import BfInterpreter (runBrainfuckProgram)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} $ do 
    describe "Brainfuck State" $ do
        it "Generates a state with no instructions" $ do
            generateProgramStartState "" `shouldBe` BrainfuckState [] 0 [0] "" 0 0
        it "Generates a state with 3 instructions" $ do
            generateProgramStartState "+++" `shouldBe` BrainfuckState [] 0 [0] "+++" 0 3 
    describe "Brainfuck Interpreter" $ do
        describe "(+) instruction" $ do
            it "Preform single instruction on clean state" $ do
                actual <- runBrainfuckProgram "+"
                actual `shouldBe` BrainfuckState [] 1 [0] "+" 1 1 
            it "Preform instruction 3 times sequentially" $ do
                actual <- runBrainfuckProgram "+++"
                actual `shouldBe` BrainfuckState [] 3 [0] "+++" 3 3
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
            generateProgramStartState "" `shouldBe` BrainfuckState [] 0 [] "" 0 0
        it "Generates a state with 3 instructions" $ do
            generateProgramStartState "+++" `shouldBe` BrainfuckState [] 0 [] "+++" 0 3 
    describe "Brainfuck Interpreter" $ do
        describe "(+) instruction" $ do
            it "Preform single instruction on clean state" $ do
                actual <- runBrainfuckProgram "+"
                actual `shouldBe` BrainfuckState [] 1 [] "+" 1 1 
            it "Preform instruction 3 times sequentially" $ do
                actual <- runBrainfuckProgram "+++"
                actual `shouldBe` BrainfuckState [] 3 [] "+++" 3 3
        describe "(-) instruction" $ do
            it "Preform single instruction on clean state" $ do
                actual <- runBrainfuckProgram "-"
                actual `shouldBe` BrainfuckState [] (-1) [] "-" 1 1 
            it "Preform instruction 3 times sequentially" $ do
                actual <- runBrainfuckProgram "---"
                actual `shouldBe` BrainfuckState [] (-3) [] "---" 3 3
        describe "(>) instruction" $ do
            it "Preform single instruction on clean state" $ do
                actual <- runBrainfuckProgram ">"
                actual `shouldBe` BrainfuckState [0] 0 [] ">" 1 1 
            it "Preform instruction 3 times sequentially" $ do
                actual <- runBrainfuckProgram ">>>"
                actual `shouldBe` BrainfuckState [0,0,0] 0 [] ">>>" 3 3
        describe "(<) instruction" $ do
            it "Preform single instruction on clean state" $ do
                actual <- runBrainfuckProgram "<"
                actual `shouldBe` BrainfuckState [] 0 [] "<" 1 1 
            it "Preform instruction 3 times sequentially, after a >>>" $ do
                actual <- runBrainfuckProgram ">>><<<"
                actual `shouldBe` BrainfuckState [] 0 [0,0,0] ">>><<<" 6 6
        describe "([]) instructions" $ do
            it "Program with only []" $ do
                actual <- runBrainfuckProgram  "[]"
                actual `shouldBe` BrainfuckState [] 0 [] "[]" 2 2
            it "Skip to ] if value at [ is 0" $ do
                actual <- runBrainfuckProgram  "[+++]"
                actual `shouldBe` BrainfuckState [] 0 [] "[+++]" 5 5
            it "Loop once (straight through)" $ do
                actual <- runBrainfuckProgram  "+[-]"
                actual `shouldBe` BrainfuckState [] 0 [] "+[-]" 4 4 
            it "Loop twice" $ do
                actual <- runBrainfuckProgram  "++[-]"
                actual `shouldBe` BrainfuckState [] 0 [] "++[-]" 5 5
        describe "More Complex Static Instruction sequences" $ do
            it "Add 4 + 8" $ do
                actual <- runBrainfuckProgram "++++>++++++++[<+>-]<"
                actual `shouldBe` BrainfuckState [] 12 [0] "++++>++++++++[<+>-]<" 20 20
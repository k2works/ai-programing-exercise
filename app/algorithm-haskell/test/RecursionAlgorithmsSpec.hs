module RecursionAlgorithmsSpec (spec) where

import Test.Hspec
import RecursionAlgorithms

spec :: Spec
spec = do
  describe "RecursionAlgorithms" $ do
    
    describe "Basic recursion" $ do
      describe "factorial" $ do
        it "calculates the factorial of 0" $ do
          factorial 0 `shouldBe` 1
        it "calculates the factorial of 1" $ do
          factorial 1 `shouldBe` 1
        it "calculates the factorial of 3" $ do
          factorial 3 `shouldBe` 6
        it "calculates the factorial of 5" $ do
          factorial 5 `shouldBe` 120
      
      describe "gcd'" $ do
        it "finds the greatest common divisor of 22 and 8" $ do
          gcd' 22 8 `shouldBe` 2
        it "finds the gcd when one number is 0" $ do
          gcd' 12 0 `shouldBe` 12
        it "finds the gcd of two equal numbers" $ do
          gcd' 7 7 `shouldBe` 7
        it "finds the gcd of coprime numbers" $ do
          gcd' 13 17 `shouldBe` 1
      
      describe "factorial' (tail recursive)" $ do
        it "calculates the factorial of 0" $ do
          factorial' 0 `shouldBe` 1
        it "calculates the factorial of 1" $ do
          factorial' 1 `shouldBe` 1
        it "calculates the factorial of 3" $ do
          factorial' 3 `shouldBe` 6
        it "calculates the factorial of 5" $ do
          factorial' 5 `shouldBe` 120
        it "produces the same result as regular factorial" $ do
          factorial' 10 `shouldBe` factorial 10
    
    describe "Hanoi Tower" $ do
      describe "hanoi" $ do
        it "solves for 0 disks" $ do
          hanoi 0 1 3 `shouldBe` []
        it "solves for 1 disk" $ do
          hanoi 1 1 3 `shouldBe` [(1, 1, 3)]
        it "solves for 2 disks" $ do
          let expected = [(1, 1, 2), (2, 1, 3), (1, 2, 3)]
          hanoi 2 1 3 `shouldBe` expected
        it "solves the Tower of Hanoi for 3 disks" $ do
          let expected = [(1, 1, 3), (2, 1, 2), (1, 3, 2), 
                         (3, 1, 3), (1, 2, 1), (2, 2, 3), (1, 1, 3)]
          hanoi 3 1 3 `shouldBe` expected
        it "generates the correct number of moves for 4 disks" $ do
          length (hanoi 4 1 3) `shouldBe` 15 -- 2^4 - 1 = 15
    
    describe "N-Queens Problem" $ do
      describe "isSafe" $ do
        it "returns True for empty board" $ do
          isSafe (1, 1) [] `shouldBe` True
        it "detects column conflict" $ do
          isSafe (2, 1) [(1, 1)] `shouldBe` False
        it "detects diagonal conflict" $ do
          isSafe (2, 2) [(1, 1)] `shouldBe` False
        it "allows safe placement" $ do
          isSafe (2, 3) [(1, 1)] `shouldBe` True
        it "checks multiple queens" $ do
          isSafe (3, 2) [(1, 5), (2, 7)] `shouldBe` True  -- (3,2)は(1,5)、(2,7)と衝突しない
          isSafe (3, 1) [(1, 1), (2, 3)] `shouldBe` False -- (3,1)は(1,1)と同じ列
      
      describe "queens" $ do
        it "finds the single solution for 1-queen problem" $ do
          length (queens 1) `shouldBe` 1
        it "finds no solution for 2-queen problem" $ do
          length (queens 2) `shouldBe` 0
        it "finds no solution for 3-queen problem" $ do
          length (queens 3) `shouldBe` 0
        it "finds solutions for 4-queen problem" $ do
          length (queens 4) `shouldBe` 2
        it "finds all 92 solutions for the 8-queens problem" $ do
          length (queens 8) `shouldBe` 92
        it "each solution has 8 queens" $ do
          let solutions = queens 8
          all (\sol -> length sol == 8) solutions `shouldBe` True

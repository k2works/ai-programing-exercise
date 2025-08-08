module ListAlgorithmsSpec (spec) where

import Test.Hspec
import ListAlgorithms

spec :: Spec
spec = do
  describe "ListAlgorithms" $ do
    
    describe "maxOf" $ do
      it "finds the maximum value in a list" $ do
        maxOf [172, 153, 192, 140, 165] `shouldBe` 192
        maxOf [-1, -5, -2] `shouldBe` (-1)
        maxOf [42] `shouldBe` 42
        maxOf [1, 1, 1] `shouldBe` 1
      
    describe "reverseList" $ do
      it "reverses the elements of a list" $ do
        let original = [2, 5, 1, 3, 9, 6, 7]
        let expected = [7, 6, 9, 3, 1, 5, 2]
        reverseList original `shouldBe` expected
        reverseList [1] `shouldBe` [1]
        reverseList ([] :: [Int]) `shouldBe` []
        reverseList "hello" `shouldBe` "olleh"
    
    describe "cardConv" $ do
      it "converts a decimal number to a given base" $ do
        cardConv 29 2 `shouldBe` "11101"
        cardConv 29 16 `shouldBe` "1D"
        cardConv 0 2 `shouldBe` "0"
        cardConv 10 2 `shouldBe` "1010"
        cardConv 255 16 `shouldBe` "FF"
        cardConv 123 10 `shouldBe` "123"
        cardConv 35 36 `shouldBe` "Z"
      
      it "handles edge cases for base conversion" $ do
        cardConv 1 2 `shouldBe` "1"
        cardConv 15 16 `shouldBe` "F"
    
    describe "primes" $ do
      it "enumerates prime numbers up to a given limit" $ do
        length (primes 1000) `shouldBe` 168 -- 1000以下の素数は168個
        primes 20 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19]
        primes 10 `shouldBe` [2, 3, 5, 7]
        primes 2 `shouldBe` [2]
        primes 1 `shouldBe` []
        primes 0 `shouldBe` []

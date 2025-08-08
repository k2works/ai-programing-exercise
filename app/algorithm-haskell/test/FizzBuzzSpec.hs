module FizzBuzzSpec (spec) where

import Test.Hspec
import FizzBuzz

spec :: Spec
spec = describe "FizzBuzz" $ do
    it "1を渡したら文字列1を返す" $
        generate 1 `shouldBe` "1"
    
    it "3を渡したら文字列Fizzを返す" $
        generate 3 `shouldBe` "Fizz"
    
    it "5を渡したら文字列Buzzを返す" $
        generate 5 `shouldBe` "Buzz"
    
    it "15を渡したら文字列FizzBuzzを返す" $
        generate 15 `shouldBe` "FizzBuzz"
    
    it "配列を返す" $
        length generateList `shouldBe` maxNumber

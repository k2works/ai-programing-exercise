module FizzBuzzSpec (spec) where

import Test.Hspec
import FizzBuzz

spec :: Spec
spec = describe "FizzBuzz" $ do
  describe "generate" $ do
    it "1を渡したら文字列1を返す" $ do
      generate 1 `shouldBe` "1"
    
    it "2を渡したら文字列2を返す" $ do
      generate 2 `shouldBe` "2"
    
    it "3を渡したらFizzを返す" $ do
      generate 3 `shouldBe` "Fizz"
    
    it "5を渡したらBuzzを返す" $ do
      generate 5 `shouldBe` "Buzz"
    
    it "15を渡したらFizzBuzzを返す" $ do
      generate 15 `shouldBe` "FizzBuzz"
    
    it "6を渡したらFizzを返す（3の倍数）" $ do
      generate 6 `shouldBe` "Fizz"
    
    it "10を渡したらBuzzを返す（5の倍数）" $ do
      generate 10 `shouldBe` "Buzz"
    
    it "30を渡したらFizzBuzzを返す（3と5の倍数）" $ do
      generate 30 `shouldBe` "FizzBuzz"

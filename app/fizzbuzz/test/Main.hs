module Main (main) where

import Test.Hspec
import FizzBuzz

main :: IO ()
main = hspec $ do
  describe "FizzBuzz" $ do
    describe "三の倍数かつ五の倍数の場合" $ do
      it "15を渡したら文字列\"FizzBuzz\"を返す" $ do
        let fizzbuzz = generate
        fizzbuzz 15 `shouldBe` "FizzBuzz"
        
    describe "三の倍数の場合" $ do
      it "3を渡したら文字列\"Fizz\"を返す" $ do
        let fizzbuzz = generate
        fizzbuzz 3 `shouldBe` "Fizz"
        
    describe "五の倍数の場合" $ do
      it "5を渡したら文字列\"Buzz\"を返す" $ do
        let fizzbuzz = generate
        fizzbuzz 5 `shouldBe` "Buzz"
        
    describe "その他の場合" $ do
      it "1を渡したら文字列\"1\"を返す" $ do
        let fizzbuzz = generate
        fizzbuzz 1 `shouldBe` "1"
        
      it "2を渡したら文字列\"2\"を返す" $ do
        let fizzbuzz = generate
        fizzbuzz 2 `shouldBe` "2"

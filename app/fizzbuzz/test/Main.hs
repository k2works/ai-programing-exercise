module Main (main) where

import Test.Hspec
import FizzBuzz (generate)

main :: IO ()
main = hspec $ do
  describe "FizzBuzz" $ do
    let fizzbuzz = generate
    
    describe "その他の場合" $ do
      it "1を渡したら文字列\"1\"を返す" $ do
        fizzbuzz 1 `shouldBe` "1"
      
      it "2を渡したら文字列\"2\"を返す" $ do
        fizzbuzz 2 `shouldBe` "2"

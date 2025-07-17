module Main (main) where

import Test.Hspec
import FizzBuzz

main :: IO ()
main = hspec $ do
  describe "FizzBuzz" $ do
    describe "タイプ1の場合" $ do
      describe "三の倍数かつ五の倍数の場合" $ do
        it "15を渡したら文字列\"FizzBuzz\"を返す" $ do
          generate Type1 15 `shouldBe` "FizzBuzz"
          
      describe "三の倍数の場合" $ do
        it "3を渡したら文字列\"Fizz\"を返す" $ do
          generate Type1 3 `shouldBe` "Fizz"
          
      describe "五の倍数の場合" $ do
        it "5を渡したら文字列\"Buzz\"を返す" $ do
          generate Type1 5 `shouldBe` "Buzz"
          
      describe "その他の場合" $ do
        it "1を渡したら文字列\"1\"を返す" $ do
          generate Type1 1 `shouldBe` "1"
          
        it "2を渡したら文字列\"2\"を返す" $ do
          generate Type1 2 `shouldBe` "2"
        
    describe "タイプ2の場合" $ do
      describe "三の倍数の場合" $ do
        it "3を渡したら文字列\"3\"を返す" $ do
          generate Type2 3 `shouldBe` "3"
          
      describe "五の倍数の場合" $ do
        it "5を渡したら文字列\"5\"を返す" $ do
          generate Type2 5 `shouldBe` "5"
          
      describe "三と五の倍数の場合" $ do
        it "15を渡したら文字列\"15\"を返す" $ do
          generate Type2 15 `shouldBe` "15"
          
      describe "その他の場合" $ do
        it "1を渡したら文字列\"1\"を返す" $ do
          generate Type2 1 `shouldBe` "1"
          
    describe "タイプ3の場合" $ do
      describe "三の倍数の場合" $ do
        it "3を渡したら文字列\"3\"を返す" $ do
          generate Type3 3 `shouldBe` "3"
          
      describe "五の倍数の場合" $ do
        it "5を渡したら文字列\"5\"を返す" $ do
          generate Type3 5 `shouldBe` "5"
          
      describe "三と五の倍数の場合" $ do
        it "15を渡したら文字列\"FizzBuzz\"を返す" $ do
          generate Type3 15 `shouldBe` "FizzBuzz"
          
      describe "その他の場合" $ do
        it "1を渡したら文字列\"1\"を返す" $ do
          generate Type3 1 `shouldBe` "1"
          
    describe "それ以外のタイプの場合" $ do
      it "TypeOther 4を渡したら空文字列を返す" $ do
        generate (TypeOther 4) 3 `shouldBe` ""

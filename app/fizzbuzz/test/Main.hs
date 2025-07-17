module Main (main) where

import Test.Hspec
import FizzBuzz (generate)

main :: IO ()
main = hspec $ do
  describe "FizzBuzz" $ do
    describe "その他の場合" $ do
      it "1を渡したら文字列\"1\"を返す" $ do
        generate 1 `shouldBe` "1"

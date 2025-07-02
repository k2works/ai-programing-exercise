module FizzBuzzSpec (spec) where

import Test.Hspec
import FizzBuzz
import Data.List (sort, sortBy)
import Data.Ord (compare)

spec :: Spec
spec = describe "FizzBuzz" $ do
  describe "三の倍数の場合" $ do
    it "3を渡したら文字列Fizzを返す" $ do
      generate 3 `shouldBe` "Fizz"

  describe "五の倍数の場合" $ do  
    it "5を渡したら文字列Buzzを返す" $ do
      generate 5 `shouldBe` "Buzz"

  describe "三と五の倍数の場合" $ do
    it "15を渡したら文字列FizzBuzzを返す" $ do
      generate 15 `shouldBe` "FizzBuzz"

  describe "その他の場合" $ do
    it "1を渡したら文字列1を返す" $ do
      generate 1 `shouldBe` "1"

  describe "リストの生成" $ do
    it "1から100までのFizzBuzzリストを生成する" $ do
      let result = generateList
      length result `shouldBe` 100
      head result `shouldBe` "1"
      result !! 2 `shouldBe` "Fizz"
      result !! 4 `shouldBe` "Buzz"
      result !! 14 `shouldBe` "FizzBuzz"

  describe "配列や繰り返し処理を理解する" $ do
    it "繰り返し処理" $ do
      let result = map (\i -> i * i) [1, 2, 3]
      result `shouldBe` [1, 4, 9]

    it "selectに相当する処理で整数だけを配列に入れて返す" $ do
      let result = filter (\x -> x `mod` 1 == 0) [1, 2, 3, 4]
      result `shouldBe` [1, 2, 3, 4]

    it "rejectに相当する処理で奇数以外を配列に入れて返す" $ do  
      let result = filter even [1, 2, 3, 4]
      result `shouldBe` [2, 4]

    it "mapで新しい要素の配列を返す" $ do
      let result = map length ["apple", "orange", "pineapple", "strawberry"]
      result `shouldBe` [5, 6, 9, 10]

    it "findに相当する処理で配列の中から条件に一致する要素を取得する" $ do
      let result = head $ filter (\x -> length x > 0) ["apple", "orange", "pineapple", "strawberry"]
      result `shouldBe` "apple"

    it "指定した評価式で並び変えた配列を返す" $ do
      let list = ["2", "4", "13", "3", "1", "10"]
      let result1 = sort list
      let result2 = sortBy (\a b -> compare (read a :: Int) (read b :: Int)) list  
      let result3 = sortBy (\a b -> compare (read b :: Int) (read a :: Int)) list
      result1 `shouldBe` ["1", "10", "13", "2", "3", "4"]
      result2 `shouldBe` ["1", "2", "3", "4", "10", "13"]
      result3 `shouldBe` ["13", "10", "4", "3", "2", "1"]

    it "grepに相当する処理で配列の中から条件に一致する要素を取得する" $ do
      let result = filter (\s -> head s == 'a') ["apple", "orange", "pineapple", "strawberry", "apricot"]
      result `shouldBe` ["apple", "apricot"]

    it "takeWhileでブロック内の条件式が真である間までの要素を返す" $ do
      let result = takeWhile (< 6) [1, 2, 3, 4, 5, 6, 7, 8, 9]
      result `shouldBe` [1, 2, 3, 4, 5]

    it "dropWhileでブロック内の条件式が真である以降の要素を返す" $ do
      let result = dropWhile (< 6) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      result `shouldBe` [6, 7, 8, 9, 10]

    it "foldlで畳み込み演算を行う" $ do
      let result = foldl (+) 0 [1, 2, 3, 4, 5]
      result `shouldBe` 15

    it "foldrで畳み込み演算を行う" $ do
      let result = foldr (+) 0 [1, 2, 3, 4, 5]  
      result `shouldBe` 15

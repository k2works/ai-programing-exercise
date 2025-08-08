module BasicAlgorithmsSpec (spec) where

import Test.Hspec
import BasicAlgorithms

spec :: Spec
spec = do
  describe "max3" $ do
    it "finds the maximum of three numbers" $ do
      max3 3 2 1 `shouldBe` 3
      max3 3 2 2 `shouldBe` 3
      max3 3 1 2 `shouldBe` 3
      max3 3 2 3 `shouldBe` 3
      max3 2 1 3 `shouldBe` 3
      max3 3 3 2 `shouldBe` 3
      max3 3 3 3 `shouldBe` 3
      max3 2 2 3 `shouldBe` 3
      max3 2 3 1 `shouldBe` 3
      max3 2 3 2 `shouldBe` 3
      max3 1 3 2 `shouldBe` 3
      max3 2 3 3 `shouldBe` 3
      max3 1 2 3 `shouldBe` 3

  describe "med3" $ do
    it "finds the median of three numbers" $ do
      med3 3 2 1 `shouldBe` 2
      med3 3 2 2 `shouldBe` 2
      med3 3 1 2 `shouldBe` 2
      med3 3 2 3 `shouldBe` 3
      med3 2 1 3 `shouldBe` 2
      med3 3 3 2 `shouldBe` 3
      med3 3 3 3 `shouldBe` 3
      med3 2 2 3 `shouldBe` 2
      med3 2 3 1 `shouldBe` 2
      med3 2 3 2 `shouldBe` 2
      med3 1 3 2 `shouldBe` 2
      med3 2 3 3 `shouldBe` 3
      med3 1 2 3 `shouldBe` 2

  describe "judgeSign" $ do
    it "judges the sign of an integer" $ do
      judgeSign 17 `shouldBe` "その値は正です。"
      judgeSign (-5) `shouldBe` "その値は負です。"
      judgeSign 0 `shouldBe` "その値は0です。"

  describe "sum1ToN" $ do
    it "calculates the sum of integers from 1 to n" $ do
      sum1ToN 5 `shouldBe` 15
      sum1ToN 10 `shouldBe` 55
      sum1ToN 0 `shouldBe` 0
      sum1ToN 1 `shouldBe` 1

  describe "alternative" $ do
    it "alternates + and - characters (method 1)" $ do
      alternative1 12 `shouldBe` "+-+-+-+-+-+-"
      alternative1 1 `shouldBe` "+"
      alternative1 2 `shouldBe` "+-"
      alternative1 0 `shouldBe` ""
    
    it "alternates + and - characters (method 2)" $ do
      alternative2 12 `shouldBe` "+-+-+-+-+-+-"
      alternative2 1 `shouldBe` "+"
      alternative2 2 `shouldBe` "+-"
      alternative2 0 `shouldBe` ""

  describe "rectangle" $ do
    it "lists the sides of rectangles with a given area" $ do
      rectangle 32 `shouldBe` ["1x32", "2x16", "4x8"]
      rectangle 12 `shouldBe` ["1x12", "2x6", "3x4"]
      rectangle 1 `shouldBe` ["1x1"]
      rectangle 16 `shouldBe` ["1x16", "2x8", "4x4"]

  describe "multiplicationTable" $ do
    it "generates a multiplication table" $ do
      let expected = unlines
            [ " 1  2  3  4  5  6  7  8  9"
            , " 2  4  6  8 10 12 14 16 18"
            , " 3  6  9 12 15 18 21 24 27"
            , " 4  8 12 16 20 24 28 32 36"
            , " 5 10 15 20 25 30 35 40 45"
            , " 6 12 18 24 30 36 42 48 54"
            , " 7 14 21 28 35 42 49 56 63"
            , " 8 16 24 32 40 48 56 64 72"
            , " 9 18 27 36 45 54 63 72 81"
            ]
      multiplicationTable `shouldBe` expected

  describe "triangleLb" $ do
    it "prints a left-bottom right-angled triangle" $ do
      let expected = unlines
            [ "*"
            , "**"
            , "***"
            , "****"
            , "*****"
            ]
      triangleLb 5 `shouldBe` expected
      
    it "handles edge cases" $ do
      triangleLb 1 `shouldBe` "*\n"
      triangleLb 0 `shouldBe` ""

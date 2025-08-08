module SortAlgorithmsSpec (spec) where

import Test.Hspec
import SortAlgorithms

spec :: Spec
spec = do
  describe "SortAlgorithms" $ do
    
    describe "bubbleSort" $ do
      it "sorts an empty list" $ do
        bubbleSort ([] :: [Int]) `shouldBe` []
      it "sorts a single element list" $ do
        bubbleSort [42] `shouldBe` [42]
      it "sorts a list of integers" $ do
        bubbleSort [6, 4, 3, 7, 1, 9, 8] `shouldBe` [1, 3, 4, 6, 7, 8, 9]
      it "sorts an already sorted list" $ do
        bubbleSort [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a reverse sorted list" $ do
        bubbleSort [5, 4, 3, 2, 1] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a list with duplicates" $ do
        bubbleSort [3, 1, 4, 1, 5, 9, 2, 6, 5] `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9]
      it "sorts a list of strings" $ do
        bubbleSort ["banana", "apple", "cherry"] `shouldBe` ["apple", "banana", "cherry"]
    
    describe "selectionSort" $ do
      it "sorts an empty list" $ do
        selectionSort ([] :: [Int]) `shouldBe` []
      it "sorts a single element list" $ do
        selectionSort [42] `shouldBe` [42]
      it "sorts a list of integers" $ do
        selectionSort [6, 4, 3, 7, 1, 9, 8] `shouldBe` [1, 3, 4, 6, 7, 8, 9]
      it "sorts an already sorted list" $ do
        selectionSort [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a reverse sorted list" $ do
        selectionSort [5, 4, 3, 2, 1] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a list with duplicates" $ do
        selectionSort [3, 1, 4, 1, 5, 9, 2, 6, 5] `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9]
      it "sorts a list of strings" $ do
        selectionSort ["banana", "apple", "cherry"] `shouldBe` ["apple", "banana", "cherry"]
    
    describe "insertionSort" $ do
      it "sorts an empty list" $ do
        insertionSort ([] :: [Int]) `shouldBe` []
      it "sorts a single element list" $ do
        insertionSort [42] `shouldBe` [42]
      it "sorts a list of integers" $ do
        insertionSort [6, 4, 3, 7, 1, 9, 8] `shouldBe` [1, 3, 4, 6, 7, 8, 9]
      it "sorts an already sorted list" $ do
        insertionSort [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a reverse sorted list" $ do
        insertionSort [5, 4, 3, 2, 1] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a list with duplicates" $ do
        insertionSort [3, 1, 4, 1, 5, 9, 2, 6, 5] `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9]
      it "sorts a list of strings" $ do
        insertionSort ["banana", "apple", "cherry"] `shouldBe` ["apple", "banana", "cherry"]
    
    describe "quickSort" $ do
      it "sorts an empty list" $ do
        quickSort ([] :: [Int]) `shouldBe` []
      it "sorts a single element list" $ do
        quickSort [42] `shouldBe` [42]
      it "sorts a list of integers" $ do
        quickSort [6, 4, 3, 7, 1, 9, 8] `shouldBe` [1, 3, 4, 6, 7, 8, 9]
      it "sorts an already sorted list" $ do
        quickSort [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a reverse sorted list" $ do
        quickSort [5, 4, 3, 2, 1] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a list with duplicates" $ do
        quickSort [3, 1, 4, 1, 5, 9, 2, 6, 5] `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9]
      it "sorts a list of strings" $ do
        quickSort ["banana", "apple", "cherry"] `shouldBe` ["apple", "banana", "cherry"]
    
    describe "mergeSort" $ do
      it "sorts an empty list" $ do
        mergeSort ([] :: [Int]) `shouldBe` []
      it "sorts a single element list" $ do
        mergeSort [42] `shouldBe` [42]
      it "sorts a list of integers" $ do
        mergeSort [5, 8, 4, 2, 6, 1, 3, 9, 7] `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]
      it "sorts an already sorted list" $ do
        mergeSort [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a reverse sorted list" $ do
        mergeSort [5, 4, 3, 2, 1] `shouldBe` [1, 2, 3, 4, 5]
      it "sorts a list with duplicates" $ do
        mergeSort [3, 1, 4, 1, 5, 9, 2, 6, 5] `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9]
      it "sorts a list of strings" $ do
        mergeSort ["banana", "apple", "cherry"] `shouldBe` ["apple", "banana", "cherry"]
    
    describe "helper functions" $ do
      describe "merge" $ do
        it "merges two empty lists" $ do
          merge ([] :: [Int]) [] `shouldBe` []
        it "merges empty list with non-empty list" $ do
          merge [] [1, 3, 5] `shouldBe` [1, 3, 5]
          merge [2, 4, 6] [] `shouldBe` [2, 4, 6]
        it "merges two sorted lists" $ do
          merge [1, 3, 5] [2, 4, 6] `shouldBe` [1, 2, 3, 4, 5, 6]
        it "merges lists with duplicates" $ do
          merge [1, 3, 3] [2, 3, 4] `shouldBe` [1, 2, 3, 3, 3, 4]
      
      describe "insert'" $ do
        it "inserts into empty list" $ do
          insert' 5 ([] :: [Int]) `shouldBe` [5]
        it "inserts at beginning" $ do
          insert' 1 [2, 3, 4] `shouldBe` [1, 2, 3, 4]
        it "inserts at end" $ do
          insert' 5 [1, 2, 3] `shouldBe` [1, 2, 3, 5]
        it "inserts in middle" $ do
          insert' 3 [1, 2, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
        it "inserts with duplicates" $ do
          insert' 3 [1, 2, 3, 4] `shouldBe` [1, 2, 3, 3, 4]
      
      describe "bubble" $ do
        it "bubbles empty list" $ do
          bubble ([] :: [Int]) `shouldBe` []
        it "bubbles single element" $ do
          bubble [42] `shouldBe` [42]
        it "bubbles ordered pair" $ do
          bubble [1, 2] `shouldBe` [1, 2]
        it "bubbles unordered pair" $ do
          bubble [2, 1] `shouldBe` [1, 2]
        it "bubbles longer list" $ do
          bubble [3, 1, 4, 2] `shouldBe` [1, 3, 2, 4]

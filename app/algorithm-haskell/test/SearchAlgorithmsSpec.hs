module SearchAlgorithmsSpec (spec) where

import Test.Hspec
import SearchAlgorithms
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "SearchAlgorithms" $ do
    
    describe "linearSearch" $ do
      it "finds the index of an element in a list" $ do
        linearSearch 2 [6, 4, 3, 2, 1, 2, 8] `shouldBe` Just 3
        linearSearch 6.4 [12.7, 3.14, 6.4, 7.2] `shouldBe` Just 2
        linearSearch "DTS" ["DTS", "AAC", "FLAC"] `shouldBe` Just 0
        linearSearch 99 [1, 2, 3] `shouldBe` Nothing
        linearSearch 5 [] `shouldBe` Nothing
      
      it "finds the first occurrence" $ do
        linearSearch 2 [2, 4, 2, 6] `shouldBe` Just 0
        linearSearch 'a' "banana" `shouldBe` Just 1
    
    describe "binarySearch" $ do
      it "finds an element in a sorted list" $ do
        binarySearch 5 [1, 2, 3, 5, 7, 8, 9] `shouldBe` Just 5
        binarySearch 1 [1, 2, 3, 5, 7, 8, 9] `shouldBe` Just 1
        binarySearch 9 [1, 2, 3, 5, 7, 8, 9] `shouldBe` Just 9
        binarySearch 6 [1, 2, 3, 5, 7, 8, 9] `shouldBe` Nothing
        binarySearch 10 [1, 2, 3, 5, 7, 8, 9] `shouldBe` Nothing
        binarySearch 0 [1, 2, 3, 5, 7, 8, 9] `shouldBe` Nothing
      
      it "handles edge cases" $ do
        binarySearch 42 [] `shouldBe` Nothing
        binarySearch 5 [5] `shouldBe` Just 5
        binarySearch 3 [5] `shouldBe` Nothing
        binarySearch 2 [1, 2] `shouldBe` Just 2
    
    describe "Data.Map operations" $ do
      let initialMap = createMap [(1, "赤尾"), (5, "武田"), (10, "小野")]
      
      it "creates a map from list" $ do
        let myMap = createMap [(1, "apple"), (2, "banana")]
        searchInMap 1 myMap `shouldBe` Just "apple"
        searchInMap 2 myMap `shouldBe` Just "banana"
        searchInMap 3 myMap `shouldBe` Nothing
      
      it "searches for a value by key" $ do
        searchInMap 1 initialMap `shouldBe` Just "赤尾"
        searchInMap 5 initialMap `shouldBe` Just "武田"
        searchInMap 10 initialMap `shouldBe` Just "小野"
        searchInMap 99 initialMap `shouldBe` Nothing
      
      it "adds a new key-value pair" $ do
        let updatedMap = addToMap 12 "鈴木" initialMap
        searchInMap 12 updatedMap `shouldBe` Just "鈴木"
        -- 元のキーも保持されている
        searchInMap 1 updatedMap `shouldBe` Just "赤尾"
      
      it "updates an existing key" $ do
        let updatedMap = addToMap 5 "新武田" initialMap
        searchInMap 5 updatedMap `shouldBe` Just "新武田"
        -- 他のキーは影響されない
        searchInMap 1 updatedMap `shouldBe` Just "赤尾"
      
      it "removes a key-value pair" $ do
        let updatedMap = removeFromMap 5 initialMap
        searchInMap 5 updatedMap `shouldBe` Nothing
        -- 他のキーは保持される
        searchInMap 1 updatedMap `shouldBe` Just "赤尾"
        searchInMap 10 updatedMap `shouldBe` Just "小野"
      
      it "handles removing non-existent key" $ do
        let updatedMap = removeFromMap 99 initialMap
        Map.size updatedMap `shouldBe` Map.size initialMap

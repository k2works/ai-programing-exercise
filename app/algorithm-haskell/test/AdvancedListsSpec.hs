module AdvancedListsSpec (spec) where

import Test.Hspec
import AdvancedLists

spec :: Spec
spec = do
  describe "AdvancedLists" $ do
    
    describe "Cyclic Lists and Lazy Evaluation" $ do
      it "creates cyclic list manually" $ do
        take 7 cyclicListManual `shouldBe` [1, 2, 3, 1, 2, 3, 1]
        take 10 cyclicListManual `shouldBe` [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]
      
      it "creates cyclic list from finite list" $ do
        let cyclic = cyclicFromList [5, 6, 7]
        take 8 cyclic `shouldBe` [5, 6, 7, 5, 6, 7, 5, 6]
      
      it "takes elements from cyclic list" $ do
        takeFromCyclic 6 [1, 2, 3] `shouldBe` [1, 2, 3, 1, 2, 3]
        takeFromCyclic 5 [10, 20] `shouldBe` [10, 20, 10, 20, 10]
      
      it "handles empty cyclic list creation" $ do
        cyclicFromList [42] `shouldBe` cycle [42]

    describe "ListZipper (Bidirectional List)" $ do
      let sampleList = [1, 2, 3, 4, 5]
      let zipper = fromList sampleList
      
      it "creates zipper from list" $ do
        toList zipper `shouldBe` sampleList
        getFocus zipper `shouldBe` Just 1
      
      it "moves forward in zipper" $ do
        let moved = goForward zipper
        getFocus moved `shouldBe` Just 2
        toList moved `shouldBe` sampleList
      
      it "moves backward in zipper" $ do
        let moved = goForward zipper
        let movedBack = goBackward moved
        getFocus movedBack `shouldBe` Just 1
        toList movedBack `shouldBe` sampleList
      
      it "handles edge cases for movement" $ do
        let emptyZipper = fromList ([] :: [Int])
        getFocus (goForward emptyZipper) `shouldBe` Nothing
        getFocus (goBackward emptyZipper) `shouldBe` Nothing
        
        let singleZipper = fromList [42]
        getFocus (goForward singleZipper) `shouldBe` Nothing
        getFocus (goBackward singleZipper) `shouldBe` Just 42
      
      it "goes to specific position" $ do
        getFocus (goToPosition 0 zipper) `shouldBe` Just 1
        getFocus (goToPosition 2 zipper) `shouldBe` Just 3
        getFocus (goToPosition 4 zipper) `shouldBe` Just 5
        getFocus (goToPosition 10 zipper) `shouldBe` Nothing  -- out of bounds
        getFocus (goToPosition (-1) zipper) `shouldBe` Just 1  -- negative position
      
      it "modifies focused element" $ do
        let modified = modify (*10) zipper
        getFocus modified `shouldBe` Just 10
        toList modified `shouldBe` [10, 2, 3, 4, 5]
        
        let emptyZipper = fromList []
        toList (modify (*2) emptyZipper) `shouldBe` []
      
      it "inserts element at focus" $ do
        let inserted = insertHere 0 zipper
        toList inserted `shouldBe` [0, 1, 2, 3, 4, 5]
        getFocus inserted `shouldBe` Just 0
      
      it "deletes element at focus" $ do
        let deleted = deleteHere zipper
        toList deleted `shouldBe` [2, 3, 4, 5]
        getFocus deleted `shouldBe` Just 2
        
        let emptyZipper = fromList ([] :: [Int])
        toList (deleteHere emptyZipper) `shouldBe` []
      
      it "performs complex zipper operations" $ do
        let z1 = fromList [10, 20, 30, 40]
        let z2 = goForward z1           -- focus on 20
        let z3 = modify (*2) z2         -- 20 -> 40
        let z4 = insertHere 25 z3       -- insert 25
        let z5 = goForward z4           -- focus on original 40
        
        getFocus z5 `shouldBe` Just 40
        toList z5 `shouldBe` [10, 25, 40, 30, 40]

    describe "List Operation Complexity" $ do
      it "describes operation complexities correctly" $ do
        listOperationComplexity PrependOp `shouldBe` "O(1) - 先頭への追加"
        listOperationComplexity AppendOp `shouldBe` "O(n) - 末尾への追加"
        listOperationComplexity HeadOp `shouldBe` "O(1) - 先頭要素の取得"
        listOperationComplexity TailOp `shouldBe` "O(1) - 先頭要素の除去"
        listOperationComplexity IndexOp `shouldBe` "O(n) - インデックスアクセス"
        listOperationComplexity LengthOp `shouldBe` "O(n) - 長さの取得"
      
      it "measures list operations" $ do
        let testList = [1, 2, 3, 4, 5]
        measureListOperation PrependOp testList `shouldBe` "Prepending element: [0,1,2,3,4,5]"
        measureListOperation AppendOp testList `shouldBe` "Appending element: [1,2,3,4,5,5]"
        measureListOperation HeadOp testList `shouldBe` "Head: 1"
        measureListOperation TailOp testList `shouldBe` "Tail: [2,3,4,5]"
        measureListOperation IndexOp testList `shouldBe` "Element at index 2: 3"
        measureListOperation LengthOp testList `shouldBe` "Length: 5"
      
      it "handles edge cases in operation measurement" $ do
        measureListOperation HeadOp [] `shouldBe` "Head of empty list: error"
        measureListOperation TailOp [] `shouldBe` "Tail of empty list: error"

    describe "Advanced List Generation" $ do
      it "generates fibonacci sequence" $ do
        take 10 fibonacciList `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
        fibonacciList !! 20 `shouldBe` 6765
      
      it "generates prime numbers" $ do
        take 10 primesList `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
        primesList !! 100 `shouldBe` 547  -- 101st prime
      
      it "generates power sequences" $ do
        take 5 (powerList 2) `shouldBe` [1, 2, 4, 8, 16]
        take 4 (powerList 3) `shouldBe` [1, 3, 9, 27]
        take 3 (powerList 10) `shouldBe` [1, 10, 100]

    describe "List Transformation and Utilities" $ do
      it "chunks lists into specified sizes" $ do
        chunksOf 3 [1..10] `shouldBe` [[1,2,3], [4,5,6], [7,8,9], [10]]
        chunksOf 2 [1..6] `shouldBe` [[1,2], [3,4], [5,6]]
        chunksOf 5 [1..3] `shouldBe` [[1,2,3]]
        chunksOf 3 ([] :: [Int]) `shouldBe` []
      
      it "interleaves two lists" $ do
        interleave [1,3,5] [2,4,6] `shouldBe` [1,2,3,4,5,6]
        interleave [1,2] [3,4,5,6] `shouldBe` [1,3,2,4,5,6]
        interleave [1,2,3,4] [5,6] `shouldBe` [1,5,2,6,3,4]
        interleave [] [1,2,3] `shouldBe` [1,2,3]
        interleave [1,2,3] [] `shouldBe` [1,2,3]
      
      it "creates sliding windows" $ do
        slidingWindow 3 [1..5] `shouldBe` [[1,2,3], [2,3,4], [3,4,5]]
        slidingWindow 2 [1..4] `shouldBe` [[1,2], [2,3], [3,4]]
        slidingWindow 5 [1..3] `shouldBe` []
        slidingWindow 1 [1..3] `shouldBe` [[1], [2], [3]]
      
      it "rotates lists left" $ do
        rotateLeft 2 [1,2,3,4,5] `shouldBe` [3,4,5,1,2]
        rotateLeft 1 [1,2,3] `shouldBe` [2,3,1]
        rotateLeft 0 [1,2,3] `shouldBe` [1,2,3]
        rotateLeft 6 [1,2,3] `shouldBe` [1,2,3]  -- full rotation
        rotateLeft 1 ([] :: [Int]) `shouldBe` []
      
      it "rotates lists right" $ do
        rotateRight 2 [1,2,3,4,5] `shouldBe` [4,5,1,2,3]
        rotateRight 1 [1,2,3] `shouldBe` [3,1,2]
        rotateRight 0 [1,2,3] `shouldBe` [1,2,3]
        rotateRight 6 [1,2,3] `shouldBe` [1,2,3]  -- full rotation
        rotateRight 1 ([] :: [Int]) `shouldBe` []

    describe "Lazy Evaluation Demonstrations" $ do
      it "works with infinite lists" $ do
        let infiniteOnes = repeat 1
        take 5 infiniteOnes `shouldBe` [1,1,1,1,1]
        
        let infiniteNaturals = [1..]
        take 10 infiniteNaturals `shouldBe` [1..10]
      
      it "demonstrates lazy evaluation with expensive computations" $ do
        let expensiveList = map (^2) [1..]  -- infinite list of squares
        take 5 expensiveList `shouldBe` [1,4,9,16,25]
        
        -- Only first 5 squares are computed, not the entire infinite list
        head expensiveList `shouldBe` 1
        expensiveList !! 3 `shouldBe` 16

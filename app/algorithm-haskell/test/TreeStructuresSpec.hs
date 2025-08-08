module TreeStructuresSpec (spec) where

import Test.Hspec
import TreeStructures
import qualified Data.Set as Set
import qualified Data.Map as Map

spec :: Spec
spec = describe "TreeStructures" $ do
  describe "Binary Tree Basic Operations" $ do
    let emptyTree = EmptyTree :: Tree Int
    let singletonTree = singleton 42
    let sampleTree = fromList [8, 6, 10, 5, 7, 9, 11]
    
    describe "Tree construction" $ do
      it "creates empty tree" $ do
        treeSize emptyTree `shouldBe` 0
        treeHeight emptyTree `shouldBe` 0
      
      it "creates singleton tree" $ do
        treeSize singletonTree `shouldBe` 1
        treeHeight singletonTree `shouldBe` 1
        treeElem 42 singletonTree `shouldBe` True
      
      it "creates tree from list" $ do
        treeSize sampleTree `shouldBe` 7
        treeElem 8 sampleTree `shouldBe` True
        treeElem 12 sampleTree `shouldBe` False

    describe "Binary Search Tree properties" $ do
      it "maintains BST property after insertion" $ do
        let tree = fromList [5, 3, 7, 2, 4, 6, 8]
        let sortedList = treeToList tree
        sortedList `shouldBe` [2, 3, 4, 5, 6, 7, 8]
      
      it "handles duplicate insertions" $ do
        let tree = fromList [5, 3, 5, 7, 3]
        treeSize tree `shouldBe` 3
        treeToList tree `shouldBe` [3, 5, 7]
      
      it "finds minimum and maximum values" $ do
        treeMin sampleTree `shouldBe` 5
        treeMax sampleTree `shouldBe` 11

    describe "Tree traversals" $ do
      let tree = fromList [4, 2, 6, 1, 3, 5, 7]
      
      it "performs in-order traversal correctly" $ do
        inOrderTraversal tree `shouldBe` [1, 2, 3, 4, 5, 6, 7]
      
      it "performs pre-order traversal correctly" $ do
        preOrderTraversal tree `shouldBe` [4, 2, 1, 3, 6, 5, 7]
      
      it "performs post-order traversal correctly" $ do
        postOrderTraversal tree `shouldBe` [1, 3, 2, 5, 7, 6, 4]

    describe "Tree deletion" $ do
      it "deletes leaf nodes" $ do
        let tree = fromList [5, 3, 7]
        let treeAfterDelete = treeDelete 3 tree
        treeElem 3 treeAfterDelete `shouldBe` False
        treeSize treeAfterDelete `shouldBe` 2
      
      it "deletes nodes with one child" $ do
        let tree = fromList [5, 3, 7, 6]
        let treeAfterDelete = treeDelete 7 tree
        treeElem 7 treeAfterDelete `shouldBe` False
        treeElem 6 treeAfterDelete `shouldBe` True
        treeSize treeAfterDelete `shouldBe` 3
      
      it "deletes nodes with two children" $ do
        let tree = fromList [5, 3, 7, 2, 4, 6, 8]
        let treeAfterDelete = treeDelete 5 tree
        treeElem 5 treeAfterDelete `shouldBe` False
        treeSize treeAfterDelete `shouldBe` 6
        -- BST property should be maintained
        treeToList treeAfterDelete `shouldBe` [2, 3, 4, 6, 7, 8]
      
      it "handles deletion from empty tree" $ do
        let tree = treeDelete 5 emptyTree
        tree `shouldBe` emptyTree

    describe "Tree properties analysis" $ do
      it "calculates tree height correctly" $ do
        treeHeight emptyTree `shouldBe` 0
        treeHeight singletonTree `shouldBe` 1
        treeHeight (fromList [1, 2, 3, 4, 5]) `shouldBe` 5  -- degenerate tree
      
      it "checks balance correctly" $ do
        let balancedTree = fromList [4, 2, 6, 1, 3, 5, 7]
        let unbalancedTree = fromList [1, 2, 3, 4, 5]
        isBalanced balancedTree `shouldBe` True
        isBalanced unbalancedTree `shouldBe` False
      
      it "calculates balance factor correctly" $ do
        let tree = Node 4 
                     (Node 2 (singleton 1) (singleton 3))
                     (singleton 6)
        balanceFactor tree `shouldBe` (-1)  -- left heavy

    describe "Edge cases" $ do
      it "handles search in empty tree" $ do
        treeElem 5 emptyTree `shouldBe` False
      
      it "handles minimum/maximum of singleton tree" $ do
        treeMin singletonTree `shouldBe` 42
        treeMax singletonTree `shouldBe` 42
      
      it "maintains BST property with negative numbers" $ do
        let tree = fromList [-5, -3, -7, -2, -4, -6, -8]
        treeToList tree `shouldBe` [-8, -7, -6, -5, -4, -3, -2]

    describe "Standard library integration" $ do
      it "converts to and from Data.Set" $ do
        let originalList = [5, 3, 7, 2, 4, 6, 8]
        let tree = fromList originalList
        let set = toSet tree
        let treeFromSet = fromSet set
        
        Set.toList set `shouldBe` [2, 3, 4, 5, 6, 7, 8]
        treeToList treeFromSet `shouldBe` [2, 3, 4, 5, 6, 7, 8]
      
      it "converts to and from Data.Map" $ do
        let originalList = [5, 3, 7]
        let tree = fromList originalList
        let mapFromTree = toMap tree
        let treeFromMap = fromMap mapFromTree
        
        Map.keys mapFromTree `shouldBe` [3, 5, 7]
        treeToList treeFromMap `shouldBe` [3, 5, 7]

    describe "Performance characteristics" $ do
      it "demonstrates BST search efficiency" $ do
        let largeTree = fromList [1..100]
        -- Even in worst case (degenerate tree), search should complete
        treeElem 50 largeTree `shouldBe` True
        treeElem 101 largeTree `shouldBe` False
      
      it "handles large tree operations" $ do
        let largeList = [1..1000]
        let tree = fromList largeList
        treeSize tree `shouldBe` 1000
        treeMin tree `shouldBe` 1
        treeMax tree `shouldBe` 1000

    describe "Complex tree operations" $ do
      it "builds and queries complex tree" $ do
        let operations = [8, 3, 10, 1, 6, 14, 4, 7, 13]
        let tree = fromList operations
        
        -- Test multiple searches
        all (`treeElem` tree) operations `shouldBe` True
        any (`treeElem` tree) [2, 5, 9, 11, 12, 15] `shouldBe` False
        
        -- Test tree structure
        treeSize tree `shouldBe` 9
        treeToList tree `shouldBe` [1, 3, 4, 6, 7, 8, 10, 13, 14]
      
      it "handles tree reconstruction" $ do
        let originalTree = fromList [5, 3, 7, 2, 4, 6, 8]
        let listFromTree = treeToList originalTree
        let reconstructedTree = fromList listFromTree
        
        -- Both trees should have same elements (though structure may differ)
        treeToList reconstructedTree `shouldBe` listFromTree
        treeSize reconstructedTree `shouldBe` treeSize originalTree

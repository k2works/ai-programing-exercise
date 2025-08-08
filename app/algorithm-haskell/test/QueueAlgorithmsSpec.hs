module QueueAlgorithmsSpec (spec) where

import Test.Hspec
import QueueAlgorithms

spec :: Spec
spec = describe "QueueAlgorithms" $ do
  describe "Queue operations" $ do
    let emptyQueue = createEmpty :: Queue Int
    
    it "is initially empty" $ do
      qIsEmpty emptyQueue `shouldBe` True

    it "is not empty after enqueue" $ do
      let q1 = enqueue 1 emptyQueue
      qIsEmpty q1 `shouldBe` False

    it "dequeues the first element" $ do
      let q1 = enqueue 1 emptyQueue
      let q2 = enqueue 2 q1
      let (val, q3) = dequeue q2
      val `shouldBe` 1
      qIsEmpty q3 `shouldBe` False

    it "handles multiple enqueues and dequeues" $ do
      let q = enqueue 3 (enqueue 2 (enqueue 1 emptyQueue)) -- 1, 2, 3の順
      let (v1, q') = dequeue q
      let (v2, q'') = dequeue q'
      let (v3, q''') = dequeue q''
      (v1, v2, v3) `shouldBe` (1, 2, 3)
      qIsEmpty q''' `shouldBe` True

    it "can peek at the front element" $ do
      let q = enqueue 3 (enqueue 2 (enqueue 1 emptyQueue))
      qPeek q `shouldBe` 1
      let (_, q') = dequeue q
      qPeek q' `shouldBe` 2

    it "handles safe operations" $ do
      dequeueSafe emptyQueue `shouldBe` Nothing
      qPeekSafe emptyQueue `shouldBe` Nothing
      let q1 = enqueue 42 emptyQueue
      dequeueSafe q1 `shouldBe` Just (42, emptyQueue)
      qPeekSafe q1 `shouldBe` Just 42

  describe "FIFO behavior" $ do
    it "follows First-In First-Out principle" $ do
      let q0 = createEmpty
      let q1 = enqueue "first" q0
      let q2 = enqueue "second" q1
      let q3 = enqueue "third" q2
      let (v1, q4) = dequeue q3
      let (v2, q5) = dequeue q4
      let (v3, q6) = dequeue q5
      (v1, v2, v3) `shouldBe` ("first", "second", "third")
      qIsEmpty q6 `shouldBe` True

  describe "Internal structure transitions" $ do
    it "handles inbox to outbox transitions correctly" $ do
      -- この場合、最初はinboxに要素が蓄積される
      let q1 = enqueue 1 createEmpty  -- inbox: [1], outbox: []
      let q2 = enqueue 2 q1           -- inbox: [2,1], outbox: []
      let q3 = enqueue 3 q2           -- inbox: [3,2,1], outbox: []
      
      -- 最初のdequeueで inbox が reverse されて outbox に移される
      let (v1, q4) = dequeue q3       -- inbox: [], outbox: [2,3] (1が取り出される)
      v1 `shouldBe` 1
      
      -- 次のdequeueはoutboxから直接取り出される
      let (v2, q5) = dequeue q4
      v2 `shouldBe` 2
      
      let (v3, q6) = dequeue q5
      v3 `shouldBe` 3
      qIsEmpty q6 `shouldBe` True

    it "handles mixed enqueue/dequeue operations" $ do
      let q1 = enqueue 1 createEmpty
      let q2 = enqueue 2 q1
      let (v1, q3) = dequeue q2
      let q4 = enqueue 3 q3
      let q5 = enqueue 4 q4
      let (v2, q6) = dequeue q5
      let (v3, q7) = dequeue q6
      let (v4, q8) = dequeue q7
      (v1, v2, v3, v4) `shouldBe` (1, 2, 3, 4)
      qIsEmpty q8 `shouldBe` True

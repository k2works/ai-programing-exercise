module StackAlgorithmsSpec (spec) where

import Test.Hspec
import StackAlgorithms

spec :: Spec
spec = describe "StackAlgorithms" $ do
  describe "Stack operations" $ do
    let emptyStack = [] :: Stack Int
    let stack1 = [1]
    let stack2 = [2, 1]
    let stack3 = [3, 2, 1]

    it "can be checked for emptiness" $ do
      isEmpty emptyStack `shouldBe` True
      isEmpty stack1 `shouldBe` False
      isEmpty stack2 `shouldBe` False

    it "can have an element pushed" $ do
      let pushed = push 4 stack3
      pushed `shouldBe` [4, 3, 2, 1]
      let pushed2 = push 0 emptyStack
      pushed2 `shouldBe` [0]

    it "can be peeked (unsafe version)" $ do
      peek stack3 `shouldBe` 3
      peek stack1 `shouldBe` 1

    it "can have an element popped (unsafe version)" $ do
      let (val, popped) = pop stack3
      val `shouldBe` 3
      popped `shouldBe` [2, 1]
      let (val2, popped2) = pop stack1
      val2 `shouldBe` 1
      popped2 `shouldBe` []

    it "can be peeked safely" $ do
      peekSafe stack3 `shouldBe` Just 3
      peekSafe stack1 `shouldBe` Just 1
      peekSafe emptyStack `shouldBe` Nothing

    it "can have an element popped safely" $ do
      popSafe stack3 `shouldBe` Just (3, [2, 1])
      popSafe stack1 `shouldBe` Just (1, [])
      popSafe emptyStack `shouldBe` Nothing

  describe "Stack with strings" $ do
    let stringStack = ["c", "b", "a"]

    it "works with string elements" $ do
      peek stringStack `shouldBe` "c"
      let (val, rest) = pop stringStack
      val `shouldBe` "c"
      rest `shouldBe` ["b", "a"]
      let newStack = push "d" stringStack
      newStack `shouldBe` ["d", "c", "b", "a"]

  describe "LIFO behavior" $ do
    it "follows Last-In First-Out principle" $ do
      let s0 = []
      let s1 = push 1 s0
      let s2 = push 2 s1
      let s3 = push 3 s2
      let (v1, s4) = pop s3
      let (v2, s5) = pop s4
      let (v3, s6) = pop s5
      (v1, v2, v3) `shouldBe` (3, 2, 1)
      isEmpty s6 `shouldBe` True

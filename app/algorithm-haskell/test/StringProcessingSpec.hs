module StringProcessingSpec (spec) where

import Test.Hspec
import StringProcessing

spec :: Spec
spec = do
  describe "StringProcessing" $ do
    
    describe "bruteForceSearch" $ do
      it "finds a substring in text" $ do
        bruteForceSearch "ABC" "XYZABCDEF" `shouldBe` True
        bruteForceSearch "Hello" "Hello, World!" `shouldBe` True
      
      it "returns False when substring not found" $ do
        bruteForceSearch "XYZ" "ABABC" `shouldBe` False
        bruteForceSearch "Missing" "Hello, World!" `shouldBe` False
      
      it "handles empty patterns" $ do
        bruteForceSearch "" "ABABC" `shouldBe` True
        bruteForceSearch "" "" `shouldBe` True
      
      it "handles empty text" $ do
        bruteForceSearch "test" "" `shouldBe` False
      
      it "finds pattern at beginning" $ do
        bruteForceSearch "Hello" "Hello World" `shouldBe` True
      
      it "finds pattern at end" $ do
        bruteForceSearch "World" "Hello World" `shouldBe` True

    describe "bruteForceSearchManual" $ do
      it "finds a substring in text" $ do
        bruteForceSearchManual "ABC" "XYZABCDEF" `shouldBe` True
        bruteForceSearchManual "Hello" "Hello, World!" `shouldBe` True
      
      it "returns False when substring not found" $ do
        bruteForceSearchManual "XYZ" "ABABC" `shouldBe` False
        bruteForceSearchManual "Missing" "Hello, World!" `shouldBe` False
      
      it "handles empty patterns" $ do
        bruteForceSearchManual "" "ABABC" `shouldBe` True
        bruteForceSearchManual "" "" `shouldBe` True
      
      it "handles empty text" $ do
        bruteForceSearchManual "test" "" `shouldBe` False
      
      it "works with numbers" $ do
        bruteForceSearchManual [1, 2, 3] [0, 1, 2, 3, 4] `shouldBe` True
        bruteForceSearchManual [5, 6] [0, 1, 2, 3, 4] `shouldBe` False

    describe "findAllOccurrences" $ do
      it "finds all occurrences of a pattern" $ do
        findAllOccurrences "AB" "ABABAB" `shouldBe` [0, 2, 4]
        findAllOccurrences "ll" "Hello, Hello" `shouldBe` [2, 9]
      
      it "returns empty list when pattern not found" $ do
        findAllOccurrences "XYZ" "ABABC" `shouldBe` []
      
      it "handles overlapping patterns" $ do
        findAllOccurrences "AA" "AAAA" `shouldBe` [0, 1, 2]
      
      it "handles empty pattern" $ do
        findAllOccurrences "" "ABC" `shouldBe` [0]
      
      it "handles single character patterns" $ do
        findAllOccurrences "A" "ABABC" `shouldBe` [0, 2]

    describe "findFirst" $ do
      it "finds first occurrence" $ do
        findFirst "AB" "ABABAB" `shouldBe` Just 0
        findFirst "ll" "Hello, Hello" `shouldBe` Just 2
      
      it "returns Nothing when not found" $ do
        findFirst "XYZ" "ABABC" `shouldBe` Nothing
      
      it "handles empty pattern" $ do
        findFirst "" "ABC" `shouldBe` Just 0

    describe "capitalize" $ do
      it "capitalizes the first letter" $ do
        capitalize "hello" `shouldBe` "Hello"
        capitalize "HELLO" `shouldBe` "Hello"
      
      it "handles empty string" $ do
        capitalize "" `shouldBe` ""
      
      it "handles single character" $ do
        capitalize "h" `shouldBe` "H"
        capitalize "H" `shouldBe` "H"
      
      it "keeps only first letter capitalized" $ do
        capitalize "hELLO" `shouldBe` "Hello"

    describe "removeSpaces" $ do
      it "removes all spaces" $ do
        removeSpaces "Hello World" `shouldBe` "HelloWorld"
        removeSpaces "  a  b  c  " `shouldBe` "abc"
      
      it "handles string without spaces" $ do
        removeSpaces "HelloWorld" `shouldBe` "HelloWorld"
      
      it "handles empty string" $ do
        removeSpaces "" `shouldBe` ""
      
      it "handles string with only spaces" $ do
        removeSpaces "   " `shouldBe` ""

    describe "reverseWords" $ do
      it "reverses word order" $ do
        reverseWords "Hello World" `shouldBe` "World Hello"
        reverseWords "The quick brown fox" `shouldBe` "fox brown quick The"
      
      it "handles single word" $ do
        reverseWords "Hello" `shouldBe` "Hello"
      
      it "handles empty string" $ do
        reverseWords "" `shouldBe` ""
      
      it "handles multiple spaces" $ do
        reverseWords "Hello    World" `shouldBe` "World Hello"

    describe "isPalindrome" $ do
      it "identifies palindromes" $ do
        isPalindrome "racecar" `shouldBe` True
        isPalindrome "A man a plan a canal Panama" `shouldBe` True
        isPalindrome "race a car" `shouldBe` False
      
      it "handles empty string" $ do
        isPalindrome "" `shouldBe` True
      
      it "handles single character" $ do
        isPalindrome "a" `shouldBe` True
      
      it "ignores case and spaces" $ do
        isPalindrome "Racecar" `shouldBe` True
        isPalindrome "A Santa at NASA" `shouldBe` True

    describe "startsWith" $ do
      it "checks if string starts with pattern" $ do
        startsWith "Hello" "Hello World" `shouldBe` True
        startsWith "Hi" "Hello World" `shouldBe` False
      
      it "handles empty pattern" $ do
        startsWith "" "Hello" `shouldBe` True
      
      it "handles exact match" $ do
        startsWith "Hello" "Hello" `shouldBe` True
      
      it "works with lists" $ do
        startsWith [1, 2] [1, 2, 3, 4] `shouldBe` True
        startsWith [3, 4] [1, 2, 3, 4] `shouldBe` False

    describe "endsWith" $ do
      it "checks if string ends with pattern" $ do
        endsWith "World" "Hello World" `shouldBe` True
        endsWith "Hello" "Hello World" `shouldBe` False
      
      it "handles empty pattern" $ do
        endsWith "" "Hello" `shouldBe` True
      
      it "handles exact match" $ do
        endsWith "Hello" "Hello" `shouldBe` True
      
      it "works with lists" $ do
        endsWith [3, 4] [1, 2, 3, 4] `shouldBe` True
        endsWith [1, 2] [1, 2, 3, 4] `shouldBe` False

    describe "countOccurrences" $ do
      it "counts occurrences of pattern" $ do
        countOccurrences "AB" "ABABAB" `shouldBe` 3
        countOccurrences "ll" "Hello, Hello" `shouldBe` 2
      
      it "returns 0 when pattern not found" $ do
        countOccurrences "XYZ" "ABABC" `shouldBe` 0
      
      it "counts overlapping patterns" $ do
        countOccurrences "AA" "AAAA" `shouldBe` 3
      
      it "handles single character" $ do
        countOccurrences "A" "ABABC" `shouldBe` 2

    describe "caesarCipher" $ do
      it "shifts letters correctly" $ do
        caesarCipher 3 "ABC" `shouldBe` "DEF"
        caesarCipher 3 "XYZ" `shouldBe` "ABC"
        caesarCipher 1 "Hello" `shouldBe` "Ifmmp"
      
      it "wraps around alphabet" $ do
        caesarCipher 25 "A" `shouldBe` "Z"
        caesarCipher 26 "A" `shouldBe` "A"
      
      it "handles negative shifts" $ do
        caesarCipher (-1) "B" `shouldBe` "A"
        caesarCipher (-3) "DEF" `shouldBe` "ABC"
      
      it "preserves non-alphabetic characters" $ do
        caesarCipher 3 "Hello, World!" `shouldBe` "Khoor, Zruog!"
      
      it "handles both cases" $ do
        caesarCipher 13 "Hello World" `shouldBe` "Uryyb Jbeyq"

    describe "rot13" $ do
      it "applies ROT13 encoding" $ do
        rot13 "Hello" `shouldBe` "Uryyb"
        rot13 "World" `shouldBe` "Jbeyq"
      
      it "is its own inverse" $ do
        rot13 (rot13 "Hello World") `shouldBe` "Hello World"
      
      it "preserves non-alphabetic characters" $ do
        rot13 "Hello, World!" `shouldBe` "Uryyb, Jbeyq!"

    describe "charFrequency" $ do
      it "counts character frequencies" $ do
        charFrequency "hello" `shouldBe` [('e', 1), ('h', 1), ('l', 2), ('o', 1)]
        charFrequency "aab" `shouldBe` [('a', 2), ('b', 1)]
      
      it "handles empty string" $ do
        charFrequency "" `shouldBe` []
      
      it "handles single character" $ do
        charFrequency "a" `shouldBe` [('a', 1)]
      
      it "sorts results alphabetically" $ do
        charFrequency "cba" `shouldBe` [('a', 1), ('b', 1), ('c', 1)]

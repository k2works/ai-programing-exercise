module Main (main) where

import Test.Hspec
import FizzBuzz

main :: IO ()
main = hspec $ do
  describe "FizzBuzz" $ do
    describe "タイプ1の場合" $ do
      describe "三の倍数かつ五の倍数の場合" $ do
        it "15を渡したら文字列\"FizzBuzz\"を返す" $ do
          generate Type1 15 `shouldBe` "FizzBuzz"
          
      describe "三の倍数の場合" $ do
        it "3を渡したら文字列\"Fizz\"を返す" $ do
          generate Type1 3 `shouldBe` "Fizz"
          
      describe "五の倍数の場合" $ do
        it "5を渡したら文字列\"Buzz\"を返す" $ do
          generate Type1 5 `shouldBe` "Buzz"
          
      describe "その他の場合" $ do
        it "1を渡したら文字列\"1\"を返す" $ do
          generate Type1 1 `shouldBe` "1"
          
        it "2を渡したら文字列\"2\"を返す" $ do
          generate Type1 2 `shouldBe` "2"
        
    describe "タイプ2の場合" $ do
      describe "三の倍数の場合" $ do
        it "3を渡したら文字列\"3\"を返す" $ do
          generate Type2 3 `shouldBe` "3"
          
      describe "五の倍数の場合" $ do
        it "5を渡したら文字列\"5\"を返す" $ do
          generate Type2 5 `shouldBe` "5"
          
      describe "三と五の倍数の場合" $ do
        it "15を渡したら文字列\"15\"を返す" $ do
          generate Type2 15 `shouldBe` "15"
          
      describe "その他の場合" $ do
        it "1を渡したら文字列\"1\"を返す" $ do
          generate Type2 1 `shouldBe` "1"
          
    describe "タイプ3の場合" $ do
      describe "三の倍数の場合" $ do
        it "3を渡したら文字列\"3\"を返す" $ do
          generate Type3 3 `shouldBe` "3"
          
      describe "五の倍数の場合" $ do
        it "5を渡したら文字列\"5\"を返す" $ do
          generate Type3 5 `shouldBe` "5"
          
      describe "三と五の倍数の場合" $ do
        it "15を渡したら文字列\"FizzBuzz\"を返す" $ do
          generate Type3 15 `shouldBe` "FizzBuzz"
          
      describe "その他の場合" $ do
        it "1を渡したら文字列\"1\"を返す" $ do
          generate Type3 1 `shouldBe` "1"
          
    describe "それ以外のタイプの場合" $ do
      it "TypeOther 4を渡したら空文字列を返す" $ do
        generate (TypeOther 4) 3 `shouldBe` ""
        
  describe "FizzBuzzValue" $ do
    describe "値オブジェクトのテスト" $ do
      it "同じ値である" $ do
        let value1 = FizzBuzzValue 1 "1"
        let value2 = FizzBuzzValue 1 "1"
        value1 `shouldBe` value2
        
      it "generateValueで値オブジェクトを返す" $ do
        let result = generateValue Type1 3
        number result `shouldBe` 3
        value result `shouldBe` "Fizz"
        
      it "show関数でデバッグ表示ができる" $ do
        let result = FizzBuzzValue 3 "Fizz"
        show result `shouldContain` "3"
        show result `shouldContain` "Fizz"
        
  describe "FizzBuzzList" $ do
    describe "ファーストクラスコレクションのテスト" $ do
      it "リストを生成できる" $ do
        let result = generateList Type1 5
        case result of
          Right list -> do
            let values = getValues list
            Prelude.length values `shouldBe` 5
            value (values !! 0) `shouldBe` "1"
            value (values !! 2) `shouldBe` "Fizz"
          Left _ -> expectationFailure "リスト生成が失敗しました"
          
      it "100件を超える場合はエラーを返す" $ do
        let result = generateList Type1 101
        case result of
          Left (OutOfRange _ msg) -> msg `shouldContain` "上限は100件までです"
          Left _ -> expectationFailure "異なるエラーが発生しました"
          Right _ -> expectationFailure "エラーが発生すべきでした"
          
      it "リストに値を追加できる" $ do
        let result1 = generateList Type1 50
        case result1 of
          Right list1 -> do
            let newValues = [generateValue Type1 n | n <- [51..100]]
            let result2 = addToList list1 newValues
            case result2 of
              Right list2 -> do
                let values = getValues list2
                Prelude.length values `shouldBe` 100
              Left _ -> expectationFailure "リスト追加が失敗しました"
          Left _ -> expectationFailure "最初のリスト生成が失敗しました"
          
  describe "例外ケース" $ do
    describe "エラーハンドリングのテスト" $ do
      it "負の値は例外を発生させる" $ do
        let result = generateValueSafe Type1 (-1)
        case result of
          Left (InvalidInput _ msg) -> msg `shouldBe` "正の値のみ有効です"
          Left _ -> expectationFailure "異なるエラーが発生しました"
          Right _ -> expectationFailure "エラーが発生すべきでした"
          
      it "0は例外を発生させる" $ do
        let result = generateValueSafe Type1 0
        case result of
          Left (InvalidInput _ msg) -> msg `shouldBe` "正の値のみ有効です"
          Left _ -> expectationFailure "異なるエラーが発生しました"
          Right _ -> expectationFailure "エラーが発生すべきでした"
          
  describe "Commandパターン" $ do
    describe "GenerateCommandのテスト" $ do
      it "GenerateCommandで値を生成できる" $ do
        let cmd = createGenerateCommand 3
        let result = runCommand cmd Type1
        case result of
          Right val -> val `shouldBe` "Fizz"
          Left _ -> expectationFailure "GenerateCommand実行が失敗しました"
          
      it "GenerateCommandでエラーハンドリングができる" $ do
        let cmd = createGenerateCommand (-1)
        let result = runCommand cmd Type1
        case result of
          Left (InvalidInput _ msg) -> msg `shouldBe` "正の値のみ有効です"
          Left _ -> expectationFailure "異なるエラーが発生しました"
          Right _ -> expectationFailure "エラーが発生すべきでした"
          
    describe "OutputCommandのテスト" $ do
      it "OutputCommandでリストを生成できる" $ do
        let cmd = createOutputCommand 5
        let result = runCommand cmd Type1
        case result of
          Right output -> do
            let lines = Prelude.lines output
            Prelude.length lines `shouldBe` 5
            lines !! 0 `shouldBe` "1"
            lines !! 2 `shouldBe` "Fizz"
          Left _ -> expectationFailure "OutputCommand実行が失敗しました"
          
      it "OutputCommandで上限チェックができる" $ do
        let cmd = createOutputCommand 101
        let result = runCommand cmd Type1
        case result of
          Left (OutOfRange _ msg) -> msg `shouldContain` "上限は100件までです"
          Left _ -> expectationFailure "異なるエラーが発生しました"
          Right _ -> expectationFailure "エラーが発生すべきでした"

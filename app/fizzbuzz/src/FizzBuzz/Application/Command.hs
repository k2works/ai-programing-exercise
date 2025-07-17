module FizzBuzz.Application.Command (
  Command,
  createGenerateCommand,
  createOutputCommand,
  runCommand
) where

import FizzBuzz.Domain.Type.FizzBuzzType
import FizzBuzz.Domain.Type.FizzBuzzError
import FizzBuzz.Domain.Model.FizzBuzzValue
import FizzBuzz.Domain.Model.FizzBuzzList
import FizzBuzz.Domain.Generator

-- コマンドの型定義：高階関数としてのコマンド
type Command = FizzBuzzType -> Either FizzBuzzError String

-- 生成コマンド作成：関数合成による純粋な変換パイプライン
createGenerateCommand :: Int -> Command
createGenerateCommand num fizzbuzzType = fmap getValue (generateValueSafe fizzbuzzType num)

-- リスト出力コマンド作成：モナド変換による関数合成
createOutputCommand :: Int -> Command
createOutputCommand count fizzbuzzType = fmap formatList (generateList fizzbuzzType count)

-- コマンド実行：関数適用の純粋な抽象化
runCommand :: Command -> FizzBuzzType -> Either FizzBuzzError String
runCommand = id

-- ヘルパー関数：関数合成によるリストフォーマット
formatList :: FizzBuzzList -> String
formatList = unlines . map getValue . getValues

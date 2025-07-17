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

-- コマンドの型定義
type Command = FizzBuzzType -> Either FizzBuzzError String

-- 生成コマンド作成
createGenerateCommand :: Int -> Command
createGenerateCommand num fizzbuzzType = 
  case generateValueSafe fizzbuzzType num of
    Right val -> Right (getValue val)
    Left err -> Left err

-- リスト出力コマンド作成
createOutputCommand :: Int -> Command
createOutputCommand count fizzbuzzType = 
  case generateList fizzbuzzType count of
    Right list -> Right (formatList list)
    Left err -> Left err

-- コマンド実行
runCommand :: Command -> FizzBuzzType -> Either FizzBuzzError String
runCommand cmd fizzbuzzType = cmd fizzbuzzType

-- ヘルパー関数：リストのフォーマット
formatList :: FizzBuzzList -> String
formatList list = unlines (map getValue (getValues list))

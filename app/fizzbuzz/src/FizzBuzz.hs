module FizzBuzz (
  -- Types
  FizzBuzzType(..), FizzBuzzValue(..), FizzBuzzList(..), FizzBuzzError(..),
  -- Commands
  FizzBuzzCommand, FizzBuzzValueCommand, FizzBuzzListCommand,
  -- Basic functions
  generate, generateValue, generateList, generateValueSafe, getValues, addToList,
  -- Command functions
  executeValueCommand, executeListCommand
) where

data FizzBuzzType = Type1 | Type2 | Type3 | TypeOther Int
  deriving (Show, Eq)

-- カスタムエラー型
data FizzBuzzError 
  = InvalidInput Int String
  | OutOfRange Int String
  | ListTooLarge Int String
  deriving (Show, Eq)

-- 値オブジェクト：数値と結果文字列をカプセル化
data FizzBuzzValue = FizzBuzzValue 
  { number :: Int
  , value :: String
  } deriving (Show, Eq)

-- ファーストクラスコレクション：FizzBuzzValueのリストをカプセル化
newtype FizzBuzzList = FizzBuzzList [FizzBuzzValue]
  deriving (Show, Eq)

-- Command型の定義（高階関数として表現）
type FizzBuzzCommand a = FizzBuzzType -> Int -> Either FizzBuzzError a

-- 具体的なCommand型
type FizzBuzzValueCommand = FizzBuzzCommand FizzBuzzValue
type FizzBuzzListCommand = FizzBuzzCommand FizzBuzzList

-- Value Command の実装
executeValueCommand :: FizzBuzzValueCommand
executeValueCommand = generateValueSafe

-- List Command の実装  
executeListCommand :: FizzBuzzListCommand
executeListCommand fizzbuzzType count = generateList fizzbuzzType count

-- スマートコンストラクタ：最大100件の制限
makeFizzBuzzList :: [FizzBuzzValue] -> Either FizzBuzzError FizzBuzzList
makeFizzBuzzList values
  | length values > 100 = Left (ListTooLarge (length values) "上限は100件までです")
  | otherwise = Right (FizzBuzzList values)

-- FizzBuzzListからリストを取得
getValues :: FizzBuzzList -> [FizzBuzzValue]
getValues (FizzBuzzList values) = values

-- FizzBuzzListに値を追加（新しいリストを返す）
addToList :: FizzBuzzList -> [FizzBuzzValue] -> Either FizzBuzzError FizzBuzzList
addToList (FizzBuzzList existing) new = makeFizzBuzzList (existing ++ new)

-- 既存のgenerate関数（互換性のため）
generate :: FizzBuzzType -> Int -> String
generate fizzbuzzType num = value $ generateValue fizzbuzzType num

-- 安全な値オブジェクト生成関数
generateValueSafe :: FizzBuzzType -> Int -> Either FizzBuzzError FizzBuzzValue
generateValueSafe _ num
  | num <= 0 = Left (InvalidInput num "正の値のみ有効です")
generateValueSafe Type1 num
  | num `mod` 15 == 0 = Right (FizzBuzzValue num "FizzBuzz")
  | num `mod` 3 == 0 = Right (FizzBuzzValue num "Fizz")
  | num `mod` 5 == 0 = Right (FizzBuzzValue num "Buzz")
  | otherwise = Right (FizzBuzzValue num (show num))
generateValueSafe Type2 num = Right (FizzBuzzValue num (show num))
generateValueSafe Type3 num
  | num `mod` 15 == 0 = Right (FizzBuzzValue num "FizzBuzz")
  | otherwise = Right (FizzBuzzValue num (show num))
generateValueSafe (TypeOther _) num = Right (FizzBuzzValue num "")

-- 新しい値オブジェクトを返す関数（unsafe版、互換性のため）
generateValue :: FizzBuzzType -> Int -> FizzBuzzValue
generateValue fizzbuzzType num = 
  case generateValueSafe fizzbuzzType num of
    Right val -> val
    Left _ -> FizzBuzzValue num ""  -- エラー時は空文字列

-- リストを生成する関数
generateList :: FizzBuzzType -> Int -> Either FizzBuzzError FizzBuzzList
generateList _ count
  | count > 100 = Left (OutOfRange count "上限は100件までです")
  | count < 1 = Left (InvalidInput count "1以上の値を指定してください")
generateList fizzbuzzType count = 
  case sequence [generateValueSafe fizzbuzzType n | n <- [1..count]] of
    Right values -> Right (FizzBuzzList values)
    Left err -> Left err
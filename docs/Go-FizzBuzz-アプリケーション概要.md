# Go FizzBuzz アプリケーション アーキテクチャ概要

## プロジェクト概要

このプロジェクトは、テスト駆動開発（TDD）によって実装されたGoのFizzBuzzアプリケーションです。Ruby入門のテスト駆動開発の手法を参考にし、Goの特性を活かして実装されています。

## 技術スタック

- **プログラミング言語**: Go 1.21
- **テストフレームワーク**: Go標準ライブラリ（testing）
- **ビルドツール**: Go modules（go.mod）

## アプリケーション構成

```
app/
├── go.mod          # Goモジュール定義
├── main.go         # プロダクトコード
└── main_test.go    # テストコード
```

## システム構成図

```plantuml
@startuml
package "FizzBuzz Application" {
  [main.go] as Main
  [main_test.go] as Test
  
  component "Core Functions" as Core {
    [Generate(int) string] as Generate
    [GenerateList(start, end int) string] as GenerateList
  }
  
  component "Main Entry Point" as Entry {
    [main()] as MainFunc
  }
  
  component "Test Suite" as TestSuite {
    [Test1を渡したら文字列1を返す] as Test1
    [Test2を渡したら文字列2を返す] as Test2
    [Test3を渡したらFizzを返す] as Test3
    [Test5を渡したらBuzzを返す] as Test5
    [Test15を渡したらFizzBuzzを返す] as Test15
    [Test1から100までのFizzBuzzを返す] as TestList
    [assertGenerate()] as Assert
  }
}

Main --> Core
Main --> Entry
Test --> TestSuite
TestSuite --> Assert
TestSuite ..> Core : テスト対象

note right of Generate
  単一の数値に対する
  FizzBuzzルールを適用
end note

note right of GenerateList
  指定範囲のFizzBuzz
  リストを生成
end note

@enduml
```

## コンポーネント詳細

### 1. Core Functions（コア機能）

#### Generate関数
- **責務**: 単一の数値に対するFizzBuzzルールの適用
- **入力**: `int` - 変換対象の数値
- **出力**: `string` - FizzBuzzルールに従った文字列
- **ロジック**:
  - 3と5の両方の倍数 → "FizzBuzz"
  - 3の倍数 → "Fizz"
  - 5の倍数 → "Buzz"
  - その他 → 数値を文字列に変換

#### GenerateList関数
- **責務**: 指定された範囲のFizzBuzzリストの生成
- **入力**: `start, end int` - 開始と終了の数値
- **出力**: `[]string` - FizzBuzzルールに従った文字列のスライス
- **ロジック**: 指定範囲の各数値に対してGenerate関数を適用

### 2. Test Suite（テストスイート）

#### テスト戦略
- **テストファースト**: プロダクトコードより先にテストを作成
- **三角測量**: 複数のテストケースで一般化を促進
- **リファクタリング**: テスト成功後のコード改善

#### テストケース構成
```plantuml
@startuml
class TestSuite {
  +assertGenerate(t, input, expected)
  +Test1を渡したら文字列1を返す()
  +Test2を渡したら文字列2を返す()
  +Test3を渡したらFizzを返す()
  +Test5を渡したらBuzzを返す()
  +Test15を渡したらFizzBuzzを返す()
  +Test1から100までのFizzBuzzを返す()
}

TestSuite --> Generate : テスト対象
TestSuite --> GenerateList : テスト対象

note right of TestSuite
  テストヘルパー関数
  重複を除去し、
  テストコードを簡潔に
end note
@enduml
```

## データフロー

```plantuml
@startuml
participant "main()" as Main
participant "GenerateList()" as GenList
participant "Generate()" as Gen
participant "fmt.Printf()" as Print

Main -> GenList: GenerateList(1, 100)
loop i = 1 to 100
  GenList -> Gen: Generate(i)
  alt i % 3 == 0 && i % 5 == 0
    Gen --> GenList: "FizzBuzz"
  else i % 3 == 0
    Gen --> GenList: "Fizz"
  else i % 5 == 0
    Gen --> GenList: "Buzz"
  else
    Gen --> GenList: string(i)
  end
end
GenList --> Main: []string

loop for each result
  Main -> Print: Printf("%d: %s\\n", i+1, result)
end
@enduml
```

## TDDサイクルの実装

### Red-Green-Refactor パターン

1. **Red（レッド）**: 失敗するテストを作成
   ```go
   func Test1を渡したら文字列1を返す(t *testing.T) {
       assertGenerate(t, 1, "1")
   }
   ```

2. **Green（グリーン）**: 最小限の実装でテスト通過
   ```go
   func Generate(number int) string {
       return "1"  // 仮実装
   }
   ```

3. **Refactor（リファクタリング）**: コードの改善
   ```go
   func Generate(number int) string {
       return fmt.Sprintf("%d", number)  // 一般化
   }
   ```

## 設計原則

### 1. 単一責任の原則
- `Generate`: 単一数値の変換のみ
- `GenerateList`: リスト生成のみ
- `main`: エントリーポイントのみ

### 2. テスタビリティ
- 純粋関数による実装
- 副作用のない設計
- ヘルパー関数による重複除去

### 3. 段階的な実装
- TODOリストによる仕様分解
- テストファーストによる確実な実装
- 三角測量による一般化

## 実行例

```bash
$ go run main.go
FizzBuzz Go implementation
1から100までのFizzBuzz:
1: 1
2: 2
3: Fizz
4: 4
5: Buzz
...
15: FizzBuzz
...
100: Buzz
```

## テスト実行

```bash
$ go test -v
=== RUN   Test1を渡したら文字列1を返す
--- PASS: Test1を渡したら文字列1を返す (0.00s)
=== RUN   Test2を渡したら文字列2を返す
--- PASS: Test2を渡したら文字列2を返す (0.00s)
...
PASS
ok      fizzbuzz        0.004s
```

## 今後の拡張性

### 考慮される拡張ポイント
1. **カスタマイズ可能なルール**: 3,5以外の倍数への対応
2. **出力形式の選択**: JSON、XML等の出力フォーマット
3. **パフォーマンス最適化**: 大きな範囲での処理効率化
4. **並行処理**: ゴルーチンを活用した並列処理
5. **CLI引数**: コマンドライン引数による範囲指定

この設計により、シンプルでテスタブル、かつ拡張可能なFizzBuzzアプリケーションを実現しています。

# Go FizzBuzz アプリケーション 実装詳細

## 実装の詳細分析

### クラス構造（Go的な実装）

Goには厳密な意味でのクラスは存在しませんが、関数とパッケージを使って構造化されています。

```plantuml
@startuml
package main {
  class FizzBuzzGenerator <<function>> {
    +Generate(number: int): string
    +GenerateList(start: int, end: int): []string
  }
  
  class TestSuite <<function>> {
    +assertGenerate(t: *testing.T, input: int, expected: string)
    +Test1を渡したら文字列1を返す(t: *testing.T)
    +Test2を渡したら文字列2を返す(t: *testing.T)
    +Test3を渡したらFizzを返す(t: *testing.T)
    +Test5を渡したらBuzzを返す(t: *testing.T)
    +Test15を渡したらFizzBuzzを返す(t: *testing.T)
    +Test1から100までのFizzBuzzを返す(t: *testing.T)
  }
  
  class MainApplication <<function>> {
    +main()
  }
}

TestSuite ..> FizzBuzzGenerator : テスト対象
MainApplication --> FizzBuzzGenerator : 使用
@enduml
```

### 初期化プロセス

```plantuml
@startuml
title Go FizzBuzz 初期化プロセス

start
:プログラム開始;
:パッケージ初期化;
:main()関数呼び出し;
:コンソールメッセージ出力;
:GenerateList(1, 100)呼び出し;
:結果の反復処理と出力;
:プログラム終了;
stop

note right
  Goの特徴:
  - シンプルな初期化
  - ガベージコレクション
  - 静的型付け
end note
@enduml
```

### FizzBuzzロジックの実装

#### Generate関数の詳細

```go
func Generate(number int) string {
    if number%3 == 0 && number%5 == 0 {
        return "FizzBuzz"
    }
    if number%3 == 0 {
        return "Fizz"
    }
    if number%5 == 0 {
        return "Buzz"
    }
    return fmt.Sprintf("%d", number)
}
```

**実装のポイント:**

1. **条件の優先順位**: 15の倍数（3と5の両方）を最初にチェック
2. **モジュロ演算子**: `%`を使用した倍数判定
3. **文字列フォーマット**: `fmt.Sprintf`による数値→文字列変換

#### 条件分岐のシーケンス図

```plantuml
@startuml
title Generate関数の処理フロー

participant "Caller" as C
participant "Generate" as G
participant "fmt.Sprintf" as F

C -> G: Generate(number)
alt number % 3 == 0 && number % 5 == 0
  G -> C: "FizzBuzz"
else number % 3 == 0
  G -> C: "Fizz"
else number % 5 == 0
  G -> C: "Buzz"
else 
  G -> F: Sprintf("%d", number)
  F -> G: string result
  G -> C: string result
end
@enduml
```

### GenerateList関数の詳細

```go
func GenerateList(start, end int) []string {
    results := make([]string, 0, end-start+1)
    for i := start; i <= end; i++ {
        results = append(results, Generate(i))
    }
    return results
}
```

**実装のポイント:**

1. **スライスの初期化**: `make([]string, 0, capacity)`で効率的なメモリ確保
2. **容量の事前計算**: `end-start+1`で必要な容量を事前計算
3. **append操作**: 動的なスライス拡張

#### GenerateListの処理フロー

```plantuml
@startuml
title GenerateList関数の処理フロー

start
:make([]string, 0, end-start+1);
:i = start;
while (i <= end?)
  :result = Generate(i);
  :append to results;
  :i++;
endwhile
:return results;
stop

note right
  計算量: O(n)
  空間計算量: O(n)
  nは範囲のサイズ
end note
@enduml
```

### テストコードの構造

#### テストヘルパー関数

```go
func assertGenerate(t *testing.T, input int, expected string) {
    t.Helper()
    got := Generate(input)
    if got != expected {
        t.Errorf("Generate(%d) = %v, want %v", input, got, expected)
    }
}
```

**設計の利点:**

1. **DRY原則**: Don't Repeat Yourself - 重複コードの除去
2. **t.Helper()**: テスト失敗時のスタックトレースを改善
3. **一貫したエラーメッセージ**: 統一されたテスト失敗報告

#### テストケースの網羅性

```plantuml
@startuml
title テストカバレッジ

package "Test Coverage" {
  [基本数値テスト] as Basic
  [3の倍数テスト] as Fizz
  [5の倍数テスト] as Buzz
  [15の倍数テスト] as FizzBuzz
  [範囲テスト] as Range
}

Basic --> Generate
Fizz --> Generate
Buzz --> Generate
FizzBuzz --> Generate
Range --> GenerateList
@enduml
```

### パフォーマンス特性

#### 時間計算量
- **Generate関数**: O(1) - 定数時間
- **GenerateList関数**: O(n) - 線形時間（nは範囲のサイズ）

#### 空間計算量
- **Generate関数**: O(1) - 定数空間
- **GenerateList関数**: O(n) - 結果スライスのサイズに比例

#### メモリ使用パターン

```plantuml
@startuml
title メモリ使用パターン

participant "main" as M
participant "GenerateList" as GL
participant "Slice" as S
participant "GC" as GC

M -> GL: GenerateList(1, 100)
GL -> S: make([]string, 0, 100)
note right: 容量100のスライス確保

loop i = 1 to 100
  GL -> GL: Generate(i)
  GL -> S: append(result)
end

GL -> M: return slice
note right: 100要素のスライス

M -> M: 処理完了
M -> GC: プログラム終了時
note right: ガベージコレクション
@enduml
```

### エラーハンドリング

現在の実装では明示的なエラーハンドリングは実装されていませんが、Goの慣習に従った拡張が可能です：

```go
// 将来の拡張例
func GenerateWithValidation(number int) (string, error) {
    if number < 1 {
        return "", fmt.Errorf("number must be positive, got %d", number)
    }
    return Generate(number), nil
}
```

### コードメトリクス

| 項目 | 値 |
|------|-----|
| 総行数 | 37行 |
| 関数数 | 3個 |
| テスト関数数 | 9個 |
| 循環的複雑度 | 4 (Generate関数) |
| テストカバレッジ | 100% |

### 実際のコード例

#### メイン処理の実行例

```go
// 実際の実行例
results := GenerateList(1, 20)
// results = ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", 
//           "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz",
//           "16", "17", "Fizz", "19", "Buzz"]
```

#### テスト実行の詳細

```bash
$ go test -v -cover
=== RUN   Test1を渡したら文字列1を返す
--- PASS: Test1を渡したら文字列1を返す (0.00s)
=== RUN   Test2を渡したら文字列2を返す
--- PASS: Test2を渡したら文字列2を返す (0.00s)
=== RUN   Test3を渡したらFizzを返す
--- PASS: Test3を渡したらFizzを返す (0.00s)
=== RUN   Test5を渡したらBuzzを返す
--- PASS: Test5を渡したらBuzzを返す (0.00s)
=== RUN   Test15を渡したらFizzBuzzを返す
--- PASS: Test15を渡したらFizzBuzzを返す (0.00s)
=== RUN   Test6を渡したらFizzを返す
--- PASS: Test6を渡したらFizzを返す (0.00s)
=== RUN   Test10を渡したらBuzzを返す
--- PASS: Test10を渡したらBuzzを返す (0.00s)
=== RUN   Test30を渡したらFizzBuzzを返す
--- PASS: Test30を渡したらFizzBuzzを返す (0.00s)
=== RUN   Test1から100までのFizzBuzzを返す
--- PASS: Test1から100までのFizzBuzzを返す (0.00s)
PASS
coverage: 100.0% of statements
ok      fizzbuzz        0.004s
```

この実装は、シンプルでありながらテスト駆動開発の原則に従い、保守性と拡張性を確保した設計となっています。

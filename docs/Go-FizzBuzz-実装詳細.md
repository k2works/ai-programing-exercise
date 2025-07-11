# Go FizzBuzz 実装詳細

## クラス構造詳細

### システム全体のクラス図

```plantuml
@startuml
!define RECTANGLE class

package "main" {
    class FizzBuzz {
        -list: *FizzBuzzList
        -fizzBuzzType: int
        -typeImpl: FizzBuzzType
        +NewFizzBuzz(fizzBuzzType: int): *FizzBuzz
        +NewFizzBuzzWithType(typeImpl: FizzBuzzType): *FizzBuzz
        +GenerateList(start: int, end: int)
        +List(): []FizzBuzzValue
        +Type(): int
        +Add(value: FizzBuzzValue)
        +Get(index: int): FizzBuzzValue
        +Count(): int
    }
}

package "domain.model" {
    class FizzBuzzList {
        -values: []FizzBuzzValue
        +NewFizzBuzzList(values: []FizzBuzzValue): *FizzBuzzList
        +Add(value: FizzBuzzValue)
        +Values(): []FizzBuzzValue
        +Count(): int
        +Get(index: int): FizzBuzzValue
    }
    
    class FizzBuzzValue {
        -number: int
        -value: string
        +NewFizzBuzzValue(number: int, value: string): FizzBuzzValue
        +Number(): int
        +Value(): string
    }
}

package "domain.type" {
    interface FizzBuzzType {
        +Generate(n: int): string
    }
    
    class FizzBuzzTypeBase {
        +Create(fizzBuzzType: int): FizzBuzzType
    }
    
    class FizzBuzzType01 {
        +Generate(n: int): string
    }
    
    class FizzBuzzType02 {
        +Generate(n: int): string
    }
    
    class FizzBuzzType03 {
        +Generate(n: int): string
    }
    
    class FizzBuzzTypeNotDefined {
        +Generate(n: int): string
    }
}

package "application" {
    class FizzBuzzCommand {
        +Execute(): interface{}
    }
    
    class FizzBuzzListCommand {
        +Execute(): interface{}
    }
    
    class FizzBuzzValueCommand {
        +Execute(): interface{}
    }
}

' Relations
FizzBuzz *-- FizzBuzzList
FizzBuzz *-- FizzBuzzType
FizzBuzzList *-- FizzBuzzValue
FizzBuzzTypeBase ..> FizzBuzzType : creates
FizzBuzzType01 ..|> FizzBuzzType
FizzBuzzType02 ..|> FizzBuzzType
FizzBuzzType03 ..|> FizzBuzzType
FizzBuzzTypeNotDefined ..|> FizzBuzzType

@enduml
```

## 初期化プロセスのシーケンス図

```plantuml
@startuml
actor User
participant "main.go" as Main
participant "FizzBuzz" as FB
participant "FizzBuzzTypeBase" as Base
participant "FizzBuzzType01" as Type01
participant "FizzBuzzList" as List

User -> Main: プログラム実行
activate Main

Main -> FB: NewFizzBuzz(TYPE_01)
activate FB

FB -> Base: Create(TYPE_01)
activate Base
Base -> Type01: new FizzBuzzType01()
activate Type01
Type01 --> Base: instance
deactivate Type01
Base --> FB: FizzBuzzType01 instance
deactivate Base

FB -> List: NewFizzBuzzList([])
activate List
List --> FB: FizzBuzzList instance
deactivate List

FB --> Main: FizzBuzz instance
deactivate FB

Main -> FB: GenerateList(1, 100)
activate FB

loop for i = 1 to 100
    FB -> Type01: Generate(i)
    activate Type01
    Type01 --> FB: result string
    deactivate Type01
    
    FB -> List: Add(FizzBuzzValue{i, result})
    activate List
    List --> FB: void
    deactivate List
end

FB --> Main: void
deactivate FB

Main -> FB: List()
activate FB
FB -> List: Values()
activate List
List --> FB: []FizzBuzzValue
deactivate List
FB --> Main: results
deactivate FB

loop for each result
    Main -> User: 結果を出力
end

deactivate Main
@enduml
```

## ゲームループのシーケンス図

```plantuml
@startuml
participant "FizzBuzz" as FB
participant "FizzBuzzType" as Type
participant "FizzBuzzList" as List
participant "FizzBuzzValue" as Value

note over FB: GenerateList(start, end) 開始

loop for number = start to end
    FB -> Type: Generate(number)
    activate Type
    
    alt number % 15 == 0
        Type --> FB: "FizzBuzz"
    else number % 3 == 0
        Type --> FB: "Fizz"
    else number % 5 == 0
        Type --> FB: "Buzz"
    else
        Type --> FB: string(number)
    end
    deactivate Type
    
    FB -> Value: NewFizzBuzzValue(number, result)
    activate Value
    Value --> FB: FizzBuzzValue instance
    deactivate Value
    
    FB -> List: Add(FizzBuzzValue)
    activate List
    List --> FB: void
    deactivate List
end

note over FB: GenerateList完了
@enduml
```

## 実装詳細

### FizzBuzzメインクラスの実装

```go
// FizzBuzz構造体
type FizzBuzz struct {
    list         *model.FizzBuzzList
    fizzBuzzType int
    typeImpl     fizzbuzztype.FizzBuzzType
}

// NewFizzBuzz コンストラクタ（プリミティブ型を受け取る）
func NewFizzBuzz(fizzBuzzType int) *FizzBuzz {
    base := fizzbuzztype.FizzBuzzTypeBase{}
    typeImpl := base.Create(fizzBuzzType)

    // 未定義タイプの場合は-1を設定
    actualType := fizzBuzzType
    if _, ok := typeImpl.(fizzbuzztype.FizzBuzzTypeNotDefined); ok {
        actualType = -1
    }

    return &FizzBuzz{
        list:         model.NewFizzBuzzList([]model.FizzBuzzValue{}),
        fizzBuzzType: actualType,
        typeImpl:     typeImpl,
    }
}
```

**設計のポイント**:
- ファクトリーパターンによるタイプインスタンス生成
- 未定義タイプの適切なハンドリング
- 依存性注入による疎結合設計

### FizzBuzzType01の実装例

```go
type FizzBuzzType01 struct{}

func (f FizzBuzzType01) Generate(n int) string {
    if n%15 == 0 {
        return "FizzBuzz"
    } else if n%3 == 0 {
        return "Fizz"
    } else if n%5 == 0 {
        return "Buzz"
    } else {
        return strconv.Itoa(n)
    }
}
```

**設計のポイント**:
- 単一責任原則：FizzBuzzロジックのみに特化
- ストラテジーパターン：アルゴリズムの交換可能性
- 明確な条件分岐による可読性

### FizzBuzzListの実装

```go
type FizzBuzzList struct {
    values []FizzBuzzValue
}

func NewFizzBuzzList(values []FizzBuzzValue) *FizzBuzzList {
    return &FizzBuzzList{values: values}
}

func (list *FizzBuzzList) Add(value FizzBuzzValue) {
    list.values = append(list.values, value)
}

func (list *FizzBuzzList) Values() []FizzBuzzValue {
    return list.values
}
```

**設計のポイント**:
- コレクション操作の抽象化
- イミュータブルな値の管理
- スレッドセーフティの考慮

## テスト実装例

### ユニットテストの構造

```go
func TestNewFizzBuzz(t *testing.T) {
    fizzbuzz := NewFizzBuzz(fizzbuzztype.TYPE_01)
    
    if fizzbuzz.Type() != fizzbuzztype.TYPE_01 {
        t.Errorf("Expected type %d, got %d", fizzbuzztype.TYPE_01, fizzbuzz.Type())
    }
    
    if fizzbuzz.Count() != 0 {
        t.Errorf("Expected count 0, got %d", fizzbuzz.Count())
    }
}

func TestGenerateList(t *testing.T) {
    fizzbuzz := NewFizzBuzz(fizzbuzztype.TYPE_01)
    fizzbuzz.GenerateList(1, 15)
    
    expected := []string{"1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz"}
    results := fizzbuzz.List()
    
    for i, result := range results {
        if result.Value() != expected[i] {
            t.Errorf("Expected %s, got %s at index %d", expected[i], result.Value(), i)
        }
    }
}
```

**テスト設計のポイント**:
- 境界値テスト（1, 15の範囲）
- 各FizzBuzzルールの検証
- エラーケースのテスト
- テストの可読性と保守性

## パフォーマンス考慮事項

### メモリ使用量の最適化
- スライスの事前容量確保
- 不要なオブジェクト生成の回避
- ガベージコレクションの負荷軽減

### 実行速度の最適化
- 条件分岐の順序最適化
- 文字列連結の効率化
- ループ処理の最適化

このアーキテクチャにより、拡張性と保守性を備えたFizzBuzzアプリケーションを実現し、Go言語でのクリーンなコード設計の実例を提供しています。

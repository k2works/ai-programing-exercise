package main

import (
	"fmt"
	"strconv"
)

// FizzBuzzType インターフェース
type FizzBuzzType interface {
	Generate(number int) string
}

// FizzBuzzTypeBase 共通処理を提供する基底構造体
type FizzBuzzTypeBase struct{}

// タイプ定数
const (
	TYPE_01 = 1
	TYPE_02 = 2
	TYPE_03 = 3
)

// Create ファクトリメソッド
func (f FizzBuzzTypeBase) Create(fizzBuzzType int) FizzBuzzType {
	switch fizzBuzzType {
	case TYPE_01:
		return FizzBuzzType01{}
	case TYPE_02:
		return FizzBuzzType02{}
	case TYPE_03:
		return FizzBuzzType03{}
	default:
		panic("該当するタイプは存在しません")
	}
}

func (f FizzBuzzTypeBase) Fizz(number int) bool {
	return number%3 == 0
}

func (f FizzBuzzTypeBase) Buzz(number int) bool {
	return number%5 == 0
}

// FizzBuzzType01 タイプ1の実装
type FizzBuzzType01 struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzType01) Generate(number int) string {
	isFizz := f.Fizz(number)
	isBuzz := f.Buzz(number)

	if isFizz && isBuzz {
		return "FizzBuzz"
	}
	if isFizz {
		return "Fizz"
	}
	if isBuzz {
		return "Buzz"
	}
	return strconv.Itoa(number)
}

// FizzBuzzType02 タイプ2の実装
type FizzBuzzType02 struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzType02) Generate(number int) string {
	return strconv.Itoa(number)
}

// FizzBuzzType03 タイプ3の実装
type FizzBuzzType03 struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzType03) Generate(number int) string {
	isFizz := f.Fizz(number)
	isBuzz := f.Buzz(number)

	if isFizz && isBuzz {
		return "FizzBuzz"
	}
	return strconv.Itoa(number)
}

// FizzBuzz構造体
type FizzBuzz struct {
	list        []string      // FizzBuzz配列を保持するフィールド
	fizzBuzzType int          // FizzBuzzのタイプを保持するフィールド
	typeImpl    FizzBuzzType  // タイプ実装
}

// NewFizzBuzz コンストラクタ（プリミティブ型を受け取る）
func NewFizzBuzz(fizzBuzzType int) *FizzBuzz {
	base := FizzBuzzTypeBase{}
	typeImpl := base.Create(fizzBuzzType)
	
	return &FizzBuzz{
		list:        make([]string, 0),
		fizzBuzzType: fizzBuzzType,
		typeImpl:    typeImpl,
	}
}

// NewFizzBuzzWithType コンストラクタ（値オブジェクトを受け取る）
func NewFizzBuzzWithType(typeImpl FizzBuzzType) *FizzBuzz {
	// タイプを逆算（実際の実装では避けるべきですが、ここでは後方互換性のため）
	var fizzBuzzType int
	switch typeImpl.(type) {
	case FizzBuzzType01:
		fizzBuzzType = TYPE_01
	case FizzBuzzType02:
		fizzBuzzType = TYPE_02
	case FizzBuzzType03:
		fizzBuzzType = TYPE_03
	default:
		panic("該当するタイプは存在しません")
	}
	
	return &FizzBuzz{
		list:        make([]string, 0),
		fizzBuzzType: fizzBuzzType,
		typeImpl:    typeImpl,
	}
}

// Generate 数を文字列に変換してFizzBuzzルールを適用
func (f *FizzBuzz) Generate(number int) string {
	return f.typeImpl.Generate(number)
}

// Type FizzBuzzタイプを取得
func (f *FizzBuzz) Type() int {
	return f.fizzBuzzType
}

// List FizzBuzz配列を取得
func (f *FizzBuzz) List() []string {
	return f.list
}

// GenerateList 範囲指定してFizzBuzzのリストを作成し、インスタンス変数に保存
func (f *FizzBuzz) GenerateList(start, end int) {
	f.list = make([]string, 0, end-start+1)
	for i := start; i <= end; i++ {
		f.list = append(f.list, f.Generate(i))
	}
}

// 後方互換性のためのラッパー関数
func Generate(number int) string {
	fizzbuzz := NewFizzBuzz(1)
	return fizzbuzz.Generate(number)
}

func GenerateByType(number, fizzBuzzType int) string {
	fizzbuzz := NewFizzBuzz(fizzBuzzType)
	return fizzbuzz.Generate(number)
}

func GenerateList(start, end int) []string {
	fizzbuzz := NewFizzBuzz(1)
	fizzbuzz.GenerateList(start, end)
	return fizzbuzz.List()
}

func main() {
	fmt.Println("FizzBuzz Game:")
	fizzbuzz := NewFizzBuzz(1)
	fizzbuzz.GenerateList(1, 100)
	results := fizzbuzz.List()
	for _, result := range results {
		fmt.Println(result)
	}
}

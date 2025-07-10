package main

import (
	"fmt"
	"strconv"
)

// FizzBuzz構造体
type FizzBuzz struct {
	list        []string // FizzBuzz配列を保持するフィールド
	fizzBuzzType int     // FizzBuzzのタイプを保持するフィールド
}

// NewFizzBuzz コンストラクタ
func NewFizzBuzz(fizzBuzzType int) *FizzBuzz {
	return &FizzBuzz{
		list:        make([]string, 0),
		fizzBuzzType: fizzBuzzType,
	}
}

// Generate 数を文字列に変換してFizzBuzzルールを適用
func (f *FizzBuzz) Generate(number int) string {
	isFizz := number%3 == 0
	isBuzz := number%5 == 0

	switch f.fizzBuzzType {
	case 1:
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
	case 2:
		return strconv.Itoa(number)
	case 3:
		if isFizz && isBuzz {
			return "FizzBuzz"
		}
		return strconv.Itoa(number)
	default:
		panic("該当するタイプは存在しません")
	}
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

package main

import (
	"fmt"
	"strconv"
)

// FizzBuzz構造体
type FizzBuzz struct{}

// NewFizzBuzz コンストラクタ
func NewFizzBuzz() *FizzBuzz {
	return &FizzBuzz{}
}

// Generate 数を文字列に変換してFizzBuzzルールを適用
func (f *FizzBuzz) Generate(number, fizzBuzzType int) string {
	switch fizzBuzzType {
	case 1:
		if number%3 == 0 && number%5 == 0 {
			return "FizzBuzz"
		}
		if number%3 == 0 {
			return "Fizz"
		}
		if number%5 == 0 {
			return "Buzz"
		}
		return strconv.Itoa(number)
	case 2:
		return strconv.Itoa(number)
	case 3:
		if number%3 == 0 && number%5 == 0 {
			return "FizzBuzz"
		}
		return strconv.Itoa(number)
	default:
		panic("該当するタイプは存在しません")
	}
}

// GenerateList 範囲指定してFizzBuzzのリストを作成
func (f *FizzBuzz) GenerateList(start, end int) []string {
	results := make([]string, 0, end-start+1)
	for i := start; i <= end; i++ {
		results = append(results, f.Generate(i, 1)) // デフォルトはタイプ1
	}
	return results
}

// 後方互換性のためのラッパー関数
func Generate(number int) string {
	fizzbuzz := NewFizzBuzz()
	return fizzbuzz.Generate(number, 1)
}

func GenerateByType(number, fizzBuzzType int) string {
	fizzbuzz := NewFizzBuzz()
	return fizzbuzz.Generate(number, fizzBuzzType)
}

func GenerateList(start, end int) []string {
	fizzbuzz := NewFizzBuzz()
	return fizzbuzz.GenerateList(start, end)
}

func main() {
	fmt.Println("FizzBuzz Game:")
	fizzbuzz := NewFizzBuzz()
	results := fizzbuzz.GenerateList(1, 100)
	for _, result := range results {
		fmt.Println(result)
	}
}

package main

import (
	"fmt"
	"strconv"
)

const MaxNumber = 100

// FizzBuzz構造体（クラス相当）
type FizzBuzz struct {
	list         []string
	fizzBuzzType FizzBuzzType
}

func main() {
	for i := 1; i <= 100; i++ {
		fmt.Println(FizzBuzzGenerate(i))
	}
}

func FizzBuzzGenerate(number int) string {
	result := strconv.Itoa(number)
	if number%3 == 0 && number%5 == 0 {
		result = "FizzBuzz"
	} else if number%3 == 0 {
		result = "Fizz"
	} else if number%5 == 0 {
		result = "Buzz"
	}
	return result
}

// NewFizzBuzz コンストラクタ（タイプを指定）
func NewFizzBuzz(fizzBuzzType int) *FizzBuzz {
	return &FizzBuzz{fizzBuzzType: createFizzBuzzType(fizzBuzzType)}
}

// NewFizzBuzzDefault デフォルトコンストラクタ（タイプ1）
func NewFizzBuzzDefault() *FizzBuzz {
	return &FizzBuzz{fizzBuzzType: createFizzBuzzType(1)}
}

// createFizzBuzzType ファクトリメソッド
func createFizzBuzzType(typeNum int) FizzBuzzType {
	switch typeNum {
	case 1:
		return &FizzBuzzType01{}
	case 2:
		return &FizzBuzzType02{}
	case 3:
		return &FizzBuzzType03{}
	default:
		panic("不正なタイプです")
	}
}

// List ゲッターメソッド（フィールドのカプセル化）
func (f *FizzBuzz) List() []string {
	return f.list
}

// Generate インスタンスメソッド（ポリモーフィズムを使用）
func (f *FizzBuzz) Generate(number int) string {
	return f.fizzBuzzType.Generate(number)
}

// GenerateList 1から100までのFizzBuzz配列を生成
func (f *FizzBuzz) GenerateList() {
	f.list = make([]string, MaxNumber)
	for i := 1; i <= MaxNumber; i++ {
		f.list[i-1] = f.Generate(i) // インスタンスのタイプを使用
	}
}

// タイプごとに出力を切り替える関数（従来の関数、後で削除予定）
func FizzBuzzGenerateWithType(number int, fizzBuzzType int) string {
	isFizz := number%3 == 0
	isBuzz := number%5 == 0

	switch fizzBuzzType {
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
		panic("不正なタイプです")
	}
}

// FizzBuzzタイプインターフェース（ポリモーフィズム）
type FizzBuzzType interface {
	Generate(number int) string
}

// FizzBuzzType01 タイプ1（通常のFizzBuzz）
type FizzBuzzType01 struct{}

func (f *FizzBuzzType01) Generate(number int) string {
	isFizz := number%3 == 0
	isBuzz := number%5 == 0

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

// FizzBuzzType02 タイプ2（数字のみ）
type FizzBuzzType02 struct{}

func (f *FizzBuzzType02) Generate(number int) string {
	return strconv.Itoa(number)
}

// FizzBuzzType03 タイプ3（FizzBuzzのみ）
type FizzBuzzType03 struct{}

func (f *FizzBuzzType03) Generate(number int) string {
	isFizz := number%3 == 0
	isBuzz := number%5 == 0

	if isFizz && isBuzz {
		return "FizzBuzz"
	}
	return strconv.Itoa(number)
}

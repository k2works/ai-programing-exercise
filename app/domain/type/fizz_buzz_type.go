package fizzbuzztype

import "fizzbuzz/domain/model"

// FizzBuzzType インターフェース
type FizzBuzzType interface {
	Generate(number int) model.FizzBuzzValue
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
		return FizzBuzzTypeNotDefined{}
	}
}

func (f FizzBuzzTypeBase) Fizz(number int) bool {
	return number%3 == 0
}

func (f FizzBuzzTypeBase) Buzz(number int) bool {
	return number%5 == 0
}

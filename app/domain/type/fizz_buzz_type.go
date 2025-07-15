package types

import "fizzbuzz/domain/model"

// FizzBuzzType FizzBuzzタイプインターフェース（ポリモーフィズム）
type FizzBuzzType interface {
	Generate(number int) *model.FizzBuzzValue
}

// FizzBuzzTypeBase 共通メソッドを提供する基底構造体
type FizzBuzzTypeBase struct{}

func (f *FizzBuzzTypeBase) IsFizz(number int) bool {
	return number%3 == 0
}

func (f *FizzBuzzTypeBase) IsBuzz(number int) bool {
	return number%5 == 0
}

// CreateFizzBuzzType ファクトリメソッド
func CreateFizzBuzzType(typeNum int) FizzBuzzType {
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

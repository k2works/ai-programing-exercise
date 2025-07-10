package fizzbuzztype

import "fizzbuzz/domain/model"

// FizzBuzzTypeNotDefined 未定義タイプの実装（Null Objectパターン）
type FizzBuzzTypeNotDefined struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzTypeNotDefined) Generate(number int) model.FizzBuzzValue {
	return model.NewFizzBuzzValue(number, "")
}

func (f FizzBuzzTypeNotDefined) String() string {
	return "未定義"
}

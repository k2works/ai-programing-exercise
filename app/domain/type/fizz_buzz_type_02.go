package types

import (
	"fizzbuzz/domain/model"
	"strconv"
)

// FizzBuzzType02 タイプ2（数字のみ）
type FizzBuzzType02 struct {
	FizzBuzzTypeBase
}

func (f *FizzBuzzType02) Generate(number int) *model.FizzBuzzValue {
	return model.NewFizzBuzzValue(number, strconv.Itoa(number))
}

package fizzbuzztype

import (
	"strconv"
	"fizzbuzz/domain/model"
)

// FizzBuzzType02 タイプ2の実装
type FizzBuzzType02 struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzType02) Generate(number int) model.FizzBuzzValue {
	return model.NewFizzBuzzValue(number, strconv.Itoa(number))
}

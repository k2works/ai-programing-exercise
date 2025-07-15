package types

import (
	"fizzbuzz/domain/model"
	"strconv"
)

// FizzBuzzType01 タイプ1（通常のFizzBuzz）
type FizzBuzzType01 struct {
	FizzBuzzTypeBase
}

func (f *FizzBuzzType01) Generate(number int) *model.FizzBuzzValue {
	if f.IsFizz(number) && f.IsBuzz(number) {
		return model.NewFizzBuzzValue(number, "FizzBuzz")
	}
	if f.IsFizz(number) {
		return model.NewFizzBuzzValue(number, "Fizz")
	}
	if f.IsBuzz(number) {
		return model.NewFizzBuzzValue(number, "Buzz")
	}
	return model.NewFizzBuzzValue(number, strconv.Itoa(number))
}

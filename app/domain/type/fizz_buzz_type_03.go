package types

import (
	"fizzbuzz/domain/model"
	"strconv"
)

// FizzBuzzType03 タイプ3（FizzBuzzのみ）
type FizzBuzzType03 struct {
	FizzBuzzTypeBase
}

func (f *FizzBuzzType03) Generate(number int) *model.FizzBuzzValue {
	if f.IsFizz(number) && f.IsBuzz(number) {
		return model.NewFizzBuzzValue(number, "FizzBuzz")
	}
	return model.NewFizzBuzzValue(number, strconv.Itoa(number))
}

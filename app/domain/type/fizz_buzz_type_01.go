package fizzbuzztype

import (
	"strconv"
	"fizzbuzz/domain/model"
)

// FizzBuzzType01 タイプ1の実装
type FizzBuzzType01 struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzType01) Generate(number int) model.FizzBuzzValue {
	isFizz := f.Fizz(number)
	isBuzz := f.Buzz(number)

	if isFizz && isBuzz {
		return model.NewFizzBuzzValue(number, "FizzBuzz")
	}
	if isFizz {
		return model.NewFizzBuzzValue(number, "Fizz")
	}
	if isBuzz {
		return model.NewFizzBuzzValue(number, "Buzz")
	}
	return model.NewFizzBuzzValue(number, strconv.Itoa(number))
}

package fizzbuzztype

import (
	"strconv"
	"fizzbuzz/domain/model"
)

// FizzBuzzType03 タイプ3の実装
type FizzBuzzType03 struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzType03) Generate(number int) model.FizzBuzzValue {
	isFizz := f.Fizz(number)
	isBuzz := f.Buzz(number)

	if isFizz && isBuzz {
		return model.NewFizzBuzzValue(number, "FizzBuzz")
	}
	return model.NewFizzBuzzValue(number, strconv.Itoa(number))
}

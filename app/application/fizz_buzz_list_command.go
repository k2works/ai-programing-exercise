package application

import (
	"fmt"
	"fizzbuzz/domain/model"
	"fizzbuzz/domain/type"
)

// FizzBuzzListCommand ファーストクラスコレクションを返すコマンド
type FizzBuzzListCommand struct {
	typeImpl fizzbuzztype.FizzBuzzType
}

// NewFizzBuzzListCommand コンストラクタ
func NewFizzBuzzListCommand(typeImpl fizzbuzztype.FizzBuzzType) *FizzBuzzListCommand {
	return &FizzBuzzListCommand{
		typeImpl: typeImpl,
	}
}

// Execute 指定した数までのFizzBuzzリストを生成して返す
func (c *FizzBuzzListCommand) Execute(number int) interface{} {
	const MaxCount = 100
	if number > MaxCount {
		panic(model.NewInvalidValueError(fmt.Sprintf("%d より多い数を許可しない", MaxCount)))
	}
	values := make([]model.FizzBuzzValue, 0, number)
	for i := 1; i <= number; i++ {
		values = append(values, c.typeImpl.Generate(i))
	}
	return model.NewFizzBuzzList(values).Value()
}

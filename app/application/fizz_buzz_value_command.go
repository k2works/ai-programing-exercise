package application

import "fizzbuzz/domain/type"

// FizzBuzzValueCommand 値オブジェクトを返すコマンド
type FizzBuzzValueCommand struct {
	typeImpl fizzbuzztype.FizzBuzzType
}

// NewFizzBuzzValueCommand コンストラクタ
func NewFizzBuzzValueCommand(typeImpl fizzbuzztype.FizzBuzzType) *FizzBuzzValueCommand {
	return &FizzBuzzValueCommand{
		typeImpl: typeImpl,
	}
}

// Execute 値オブジェクトの値部分を返す
func (c *FizzBuzzValueCommand) Execute(number int) interface{} {
	return c.typeImpl.Generate(number).Value()
}

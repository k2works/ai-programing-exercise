package application

import "fizzbuzz/domain/type"

// FizzBuzzValueCommand 値オブジェクトを返すコマンド
type FizzBuzzValueCommand struct {
	fizzBuzzType types.FizzBuzzType
}

// NewFizzBuzzValueCommand FizzBuzzValueCommandのコンストラクタ
func NewFizzBuzzValueCommand(fizzBuzzType types.FizzBuzzType) *FizzBuzzValueCommand {
	return &FizzBuzzValueCommand{fizzBuzzType: fizzBuzzType}
}

// Execute 値オブジェクトを生成して値を返す
func (cmd *FizzBuzzValueCommand) Execute(number int) string {
	value := cmd.fizzBuzzType.Generate(number)
	return value.Value()
}

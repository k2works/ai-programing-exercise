package application

import (
	"fizzbuzz/domain/model"
	"fizzbuzz/domain/type"
)

// FizzBuzzListCommand リストを返すコマンド
type FizzBuzzListCommand struct {
	fizzBuzzType types.FizzBuzzType
}

// NewFizzBuzzListCommand FizzBuzzListCommandのコンストラクタ  
func NewFizzBuzzListCommand(fizzBuzzType types.FizzBuzzType) *FizzBuzzListCommand {
	return &FizzBuzzListCommand{fizzBuzzType: fizzBuzzType}
}

// Execute リストを生成して返す
func (cmd *FizzBuzzListCommand) Execute(maxNumber int) []string {
	values := make([]*model.FizzBuzzValue, maxNumber)
	for i := 1; i <= maxNumber; i++ {
		values[i-1] = cmd.fizzBuzzType.Generate(i)
	}
	list := model.NewFizzBuzzList(values)
	result := make([]string, list.Count())
	for i := 0; i < list.Count(); i++ {
		result[i] = list.Get(i).Value()
	}
	return result
}

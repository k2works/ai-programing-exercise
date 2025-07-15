package main

import (
	"fizzbuzz/application"
	types "fizzbuzz/domain/type"
	"fmt"
	"strconv"
)

func main() {
	// 新しいアーキテクチャを使用したFizzBuzz
	command := application.NewFizzBuzzListCommand(types.CreateFizzBuzzType(1))
	result := command.Execute(100)
	for _, value := range result {
		fmt.Println(value)
	}
}

// 従来の関数（後方互換性のため残存）
func FizzBuzzGenerate(number int) string {
	result := strconv.Itoa(number)
	if number%3 == 0 && number%5 == 0 {
		result = "FizzBuzz"
	} else if number%3 == 0 {
		result = "Fizz"
	} else if number%5 == 0 {
		result = "Buzz"
	}
	return result
}

// タイプごとに出力を切り替える関数（従来の関数、後で削除予定）
func FizzBuzzGenerateWithType(number int, fizzBuzzType int) string {
	isFizz := number%3 == 0
	isBuzz := number%5 == 0

	switch fizzBuzzType {
	case 1:
		if isFizz && isBuzz {
			return "FizzBuzz"
		}
		if isFizz {
			return "Fizz"
		}
		if isBuzz {
			return "Buzz"
		}
		return strconv.Itoa(number)
	case 2:
		return strconv.Itoa(number)
	case 3:
		if isFizz && isBuzz {
			return "FizzBuzz"
		}
		return strconv.Itoa(number)
	default:
		panic("不正なタイプです")
	}
}

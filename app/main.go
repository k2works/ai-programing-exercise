package main

import (
	"fmt"
	"strconv"
)

func main() {
	for i := 1; i <= 100; i++ {
		fmt.Println(FizzBuzzGenerate(i))
	}
}

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

// タイプごとに出力を切り替える関数（デフォルト引数の代わりにオーバーロード）
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

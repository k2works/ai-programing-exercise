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

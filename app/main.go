package main

import "fmt"

// FizzBuzzクラスに相当する関数
func Generate(number int) string {
	if number%3 == 0 && number%5 == 0 {
		return "FizzBuzz"
	}
	if number%3 == 0 {
		return "Fizz"
	}
	if number%5 == 0 {
		return "Buzz"
	}
	return fmt.Sprintf("%d", number)
}

func main() {
	fmt.Println("FizzBuzz Go implementation")
}

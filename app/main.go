package main

import "strconv"

func FizzBuzzGenerate(number int) string {
	result := strconv.Itoa(number)
	if number%3 == 0 {
		result = "Fizz"
	} else if number%5 == 0 {
		result = "Buzz"
	}
	return result
}

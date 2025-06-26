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

// 指定された範囲のFizzBuzzリストを生成する関数
func GenerateList(start, end int) []string {
	results := make([]string, 0, end-start+1)
	for i := start; i <= end; i++ {
		results = append(results, Generate(i))
	}
	return results
}

func main() {
	fmt.Println("FizzBuzz Go implementation")
	fmt.Println("1から100までのFizzBuzz:")
	
	results := GenerateList(1, 100)
	for i, result := range results {
		fmt.Printf("%d: %s\n", i+1, result)
	}
}

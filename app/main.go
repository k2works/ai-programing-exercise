package main

import "fmt"

func main() {
	fmt.Println("FizzBuzz Game:")
	fizzbuzz := NewFizzBuzz(1)
	fizzbuzz.GenerateList(1, 100)
	results := fizzbuzz.List()
	for _, result := range results {
		fmt.Println(result.Value())
	}
}

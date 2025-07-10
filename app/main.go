package main

import (
	"fmt"
	"fizzbuzz/domain/type"
)

func main() {
	fmt.Println("FizzBuzz Game:")
	fizzbuzz := NewFizzBuzz(fizzbuzztype.TYPE_01)
	fizzbuzz.GenerateList(1, 100)
	results := fizzbuzz.List()
	for _, result := range results {
		fmt.Println(result.Value())
	}
}

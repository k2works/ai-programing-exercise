package main

import (
	"fizzbuzz/application"
	types "fizzbuzz/domain/type"
	"fmt"
)

func main() {
	// 新しいアーキテクチャを使用したFizzBuzz
	command := application.NewFizzBuzzListCommand(types.CreateFizzBuzzType(1))
	result := command.Execute(100)
	for _, value := range result {
		fmt.Println(value)
	}
}
